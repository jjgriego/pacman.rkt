#lang htdp/isl+

(require 2htdp/image)
(require 2htdp/universe)

;;< Utilities
; K-combinator
; X -> Y -> X
(define (constantly c)
  (lambda (x) c))

; Int X -> [X]
; Builds a list of `n' copies of x
(define (repeat n x)
  (build-list n (constantly x)))

; [X] Int X -> [X]
; Reassigns the `i'th element of `l' to x
(define (list-set l i x)
  (cond [(empty? l) l]
        [(zero? i) (cons x (rest l))]
        [else (cons (first l) (list-set (rest l) (sub1 i) x))]))

(define-struct thunk ())

; Thunk -> Color
; Creates a random Color
(define (random-color thunk)
  (make-color (random 256)
              (random 256)
              (random 256)))

(define (filter-not pred l)
  (filter (lambda (x) (not (pred x))) l))

(define (take n l)
  (cond [(empty? l) empty]
        [(zero? n) empty]
        [else (cons (first l) (take (sub1 n) (rest l)))]))

(define (drop n l)
  (cond [(empty? l) empty]
        [(zero? n) l]
        [else (drop (sub1 n) (rest l))]))

(define (partition n l)
  (cond [(< (length l) n) empty]
        [else             (cons (take n l) (partition n (drop n l)))]))

(define (find l pred)
  (cond [(empty? l) #f]
        [(pred (first l)) (first l)]
        [else (find (rest l) pred)]))

(define (find-idx l pred)
  (cond [(empty? l) #f]
        [(pred (first l)) 0]
        [else (add1 (find-idx (rest l) pred))]))

(define (sum l) (foldl + 0 l))
(define (prod l) (foldl * 1 l))

(define (direction-reverse d)
  (cond [(symbol=? d 'north) 'south]
        [(symbol=? d 'south) 'north]
        [(symbol=? d 'east) 'west]
        [(symbol=? d 'west) 'east]))

;;>
;;< Basic grid (2d-array) abstractions

; data Grid X where
;   make-grid : Int [X] -> Grid X
;               {exists n:Int (length contents) = (* n width)}
(define-struct grid (width contents))

(define example-grid (make-grid 3 '(1 1 1 1 1 1 1 1 1)))

; Grid X -> Int
; Determines the height of a grid
(define (grid-height g)
  (/ (length (grid-contents g))
     (grid-width g)))

; (Grid X) Int Int -> Int
; Gets the index for the grid's internal list
(define (grid-idx g i j)
  (+ (* (grid-width g) j)
     i))

; (Grid X) Int Int -> X
; 0 <= i < (grid-width g)
; 0 <= j < (grid-height g)
; Gets the item at i,j in the grid
(define (grid-ref g i j)
  (list-ref (grid-contents g)
            (+ (* (grid-width g) j)
               i)))

; (Grid X) Int Int X -> (Grid X)
; Reassigns the element at i,j to x
(define (grid-set g i j x)
  (make-grid (grid-width g)
             (list-set (grid-contents g)
                       (grid-idx g i j)
                       x)))

; (Grid X) (X -> Y) -> Grid Y
; Applies `f' to each element in the grid
(define (grid-map g f)
  (make-grid (grid-width g)
             (map f (grid-contents g))))

; (Grid Y) (X Y -> X) X -> X
; Folds `f' over every element of the grid
(define (grid-fold g f x0)
  (foldl f x0 (grid-contents g)))

; (Grid Y) ([Y] -> X) ([X] -> Z) -> Z
; Folds over each row of the grid, then folds over the rows
(define (grid-scan g fi fj)
  (fj (map fi (partition (grid-width g) (grid-contents g)))))

; (Grid X) (X Int Int -> Y) -> Grid Y
; Applies `f' to each element in the grid and its coordinates
(define (grid-indexed-map g f)
  (let* ([width (grid-width g)]
         [contents (grid-contents g)]
         [height (/ (length contents) width)])
    (make-grid width
               (map f
                    contents
                    (apply append
                           (repeat height
                                   (build-list width identity)))
                    (apply append
                           (build-list height
                                       (lambda (n) (repeat width n))))))))

;;>
;;< Drawing abstractions

; type Drawing = Image -> Image

; X [(X -> X)] -> X
; Applies every function in the list to x
(define (apply-each x0 fs)
  (foldl (lambda (f x) (f x)) x0 fs))

; Image [Drawing] -> Image
; Applies all drawings to the base image
(define apply-drawings apply-each)

; [Drawing] -> Drawing
; Combines a list of drawings into a single drawing
(define (combine-drawings ds)
  (foldl compose identity ds))


; Image Int Int -> Drawing
; Creates a drawing that draws the image at the point
(define (draw-at img x y)
  (lambda (i0)
    (place-image/align img
                       x y
                       "left" "top"
                       i0)))

; Image Int Int -> Drawing
; Creates a drawing that draws the image centered at the point
(define (draw-at/center img x y)
  (lambda (i0)
    (place-image img
                 x y
                 i0)))

; Image Int Int -> Drawing
; Draws the image with its upper-left corner at the given maze cell
(define (draw-at-cell img i j)
  (draw-at img
           (* i maze-cell-size)
           (* j maze-cell-size)))

;;>
;;< Constants

; Image
(define maze-image (bitmap/file "pacman-maze.png"))

; Grid Bool
(define default-maze (make-grid (image-width maze-image)
                                (map (lambda (c) (equal? c (make-color 255 255 255)))
                                     (image->color-list maze-image))))

; type Maze = Grid Bool

; Int
; The width in cells of the maze
(define maze-width (grid-width default-maze))
; Int
; The height in cells of the maze
(define maze-height (grid-height default-maze))

; Int
; The width/height of a maze cell, in pixels
(define maze-cell-size 16)

; Int
; The width in pixels of the maze
(define maze-width-pixels (* maze-width maze-cell-size))
; Int
; The height in pixels of the maze
(define maze-height-pixels (* maze-height maze-cell-size))

; Int
; Half the width of a single maze cell
(define half-cell (/ maze-cell-size 2))
; Int
; The color of the paths Pacman can travel
(define maze-cell-color (make-color 0 0 60))

;;>
;;< Cell functions
; data Cell = Cell Int Int
; a cell in the maze

(define-struct cell (i j))

; Cell
; the zero cell
(define cell-zero (make-cell 0 0))

; Cell Cell -> Cell
; component-wise addition on a torus
(define (cell-add c1 c2)
  (make-cell (remainder (+ (cell-i c1) (cell-i c2) maze-width) maze-width)
             (remainder (+ (cell-j c1) (cell-j c2) maze-height) maze-height)))

; Cell -> Cell
; Places a cell back on the torus
(define (cell-wrap c)
  (cell-add c cell-zero))

; Direction -> Cell
; Get the unit cell in a direction
(define (cell-direction dir)
  (cond [(symbol=? dir 'north) (make-cell 0 -1)]
        [(symbol=? dir 'south) (make-cell 0 1)]
        [(symbol=? dir 'east) (make-cell 1 0 )]
        [(symbol=? dir 'west) (make-cell -1 0)]))

; Int
; The number of pixels pacman moves in one frame
(define pacman-speed 3)
; Int
; The number of pixels ghosts move in one frame
(define ghost-speed 2)

;;>
;;< Generating maze structures

;-------------------------------------------------------------------------------
; Our main goal here is to prepare the maze structure both for rendering and for
; easy searching/traversal when we need to manage the movement of the ghosts.
;
; At a high level, we're turning the maze into a weighted graph and assigning a
; nearest-node to each cell that pacman or a ghost can be in.
;-------------------------------------------------------------------------------

; Maze Int Int -> Direction -> Maybe Direction
; Get a predicate to check if a cell has a neighbor in a given direction
(define (find-neighbor m i j)
  (let ([cell (make-cell i j)])
    (lambda (direction)
      (let* ([d (cell-direction direction)]
             [new-cell (cell-add cell d)])
          (if (grid-ref m
                        (cell-i new-cell)
                        (cell-j new-cell))
            false
            direction)))))

; Maze -> Bool Int Int -> [Direction]
; Gets a list of the neighbors at a point i,j
(define (cell-neighbors m)
  (lambda (x0 i j)
    (if x0
      empty
      (filter-not false? (map (find-neighbor m i j) '(north south east west))))))

; Maze -> Grid [Direction]
(define (maze-neighbors m)
  (grid-indexed-map m (cell-neighbors m)))

; Grid [Direction]
(define maze-cell-neighbors (maze-neighbors default-maze))

(define-struct intersection (i j directions))

; [Intersection]
(define maze-intersections
  (filter-not false?
              (grid-contents (grid-indexed-map maze-cell-neighbors
                                               (lambda (c i j)
                                                 (if (> (length c) 2)
                                                   (make-intersection i j c)
                                                   #f))))))

; Cell -> Maybe Intersection
(define (get-intersection cell)
  (find maze-intersections
        (lambda (int)
          (and (= (intersection-i int) (cell-i cell))
               (= (intersection-j int) (cell-j cell))))))

; Cell Direction -> Int*
; Gets the intersection number reachable from a given cell, ignoring those in a
; direction dir
;
; This function is partial
(define (maze-intersection-traverse cell dir)
  (let ([int (get-intersection cell)])
    (if (false? int)
      (let* ([last-dir (direction-reverse dir)]
             [next-dir (find (grid-ref maze-cell-neighbors
                                       (cell-i cell)
                                       (cell-j cell))
                             (lambda (d)
                               (not (symbol=? last-dir d))))])
          (let ([delta (cell-direction next-dir)])
            (maze-intersection-traverse (cell-add cell delta)
                                        next-dir)))
      (find-idx maze-intersections
                (lambda (x) (equal? x int))))))

; Cell Direction -> Int*
; Gets the connection from an intersection in a given direction
;
; This function is partial
(define (maze-intersection-connection cell dir)
  (let ([delta (cell-direction dir)])
    (maze-intersection-traverse (cell-add cell delta)
                                dir)))

(define-struct connection (direction intersection-num))
(define-struct intersection-info (i j connections))

; Intersection -> IntersectionInfo
; Populates a intersection with the ids of the intersections reachable directly
; from it
(define (maze-intersection-connections int)
  (let* ([i (intersection-i int)]
         [j (intersection-j int)]
         [connections (map (lambda (d)
                             (make-connection d
                                              (maze-intersection-connection
                                                (make-cell i j)
                                                d)))
                           (intersection-directions int))])
    (make-intersection-info i j connections)))

; [IntersectionInfo]
(define maze-connections (map maze-intersection-connections
                              maze-intersections))


; Image
; An image of the maze
(define rendered-maze
  (let* ([maze-cells (grid-map default-maze
                               (lambda (c)
                                 (if c
                                     empty-image
                                     (rectangle maze-cell-size maze-cell-size
                                                  'solid maze-cell-color))))])
    (freeze
      (apply-drawings (rectangle (* maze-cell-size (grid-width default-maze))
                                 (* maze-cell-size (grid-height default-maze))
                                 'solid 'black)
                      (grid-contents (grid-indexed-map maze-cells draw-at-cell))))))

; data Direction = 'north | 'south | 'east | 'west

;;>
;;< Offset functions
; data Offset = Offset Int Int

(define-struct offset (x y))

; Offset
; the zero offset
(define offset-zero (make-offset 0 0))

; Offset Offset -> Offset
; component-wise addition on a torus
(define (offset-add o1 o2)
  (make-offset (remainder (+ (offset-x o1) (offset-x o2) maze-width-pixels)
                          maze-width-pixels)
               (remainder (+ (offset-y o1) (offset-y o2) maze-height-pixels)
                          maze-height-pixels))) 

; Offset -> Offset
; Places an offset back on the torus
(define (offset-wrap o)
  (offset-add o offset-zero))

; Offset Num -> Offset
; Scales an offset
(define (offset-scale s o)
  (offset-wrap (make-offset (* s (offset-x o))
                            (* s (offset-y o)))))

; Offset Offset -> Offset
; Gets the difference between two offsets
(define (offset-sub o1 o2)
  (offset-wrap (make-offset (- (offset-x o1) (offset-x o2))
                            (- (offset-y o1) (offset-y o2)))))

; Cell -> Offset
; Gets the coordinates of a cell's upeper-left as an offset
(define (cell->offset c)
  (offset-wrap (make-offset (* maze-cell-size (cell-i c))
                            (* maze-cell-size (cell-j c)))))

; Offset -> Cell
; Returns the cell an offset lies in
(define (offset->cell o)
  (cell-wrap (make-cell (floor (/ (offset-x o) maze-cell-size))
                        (floor (/ (offset-y o) maze-cell-size)))))


; Cell Offset -> Offset
; Gets the total offset given a cell and an offset from that cell's upper-left
(define (cell-offset c o)
  (offset-add (cell->offset c)
                           o))

; Direction -> Offset
; Gets the unit offset in a direction
(define (offset-direction dir)
  (cond
    [(symbol=? dir 'north) (make-offset 0 -1)]
    [(symbol=? dir 'south) (make-offset 0 1)]
    [(symbol=? dir 'east) (make-offset 1 0)]
    [(symbol=? dir 'west) (make-offset -1 0)]))

; Offset Direction -> Offset
; For an offset inside of a cell, projects the offset onto the axis in the
; direction given
(define (center-offset o dir)
  (cond
    [(symbol=? dir 'north) (make-offset half-cell (offset-y o))]
    [(symbol=? dir 'south) (make-offset half-cell (offset-y o))]
    [(symbol=? dir 'east) (make-offset (offset-x o) half-cell)]
    [(symbol=? dir 'west) (make-offset (offset-x o) half-cell)]))

;;>
;;< Position functions
; data Position = Position Cell Offset
;
; The offset must lie in the square with corners
; (0, 0) - (maze-cell-size, maze-cell,size)

(define-struct position (cell offset))

; Position -> Position
; Normalizes a position so it meets the constraints given above
(define (reconcile-position p)
  (let* ([total     (cell-offset (position-cell p)
                                 (position-offset p))]
         [real-cell (offset->cell total)]
         [new-offset (offset-sub total (cell->offset real-cell))])
    (make-position real-cell new-offset)))

; Position Offset -> Position
; Moves a position by the given offset
(define (position-move p o)
  (reconcile-position
    (make-position (position-cell p)
                   (offset-add (position-offset p)
                               o))))

; Position -> Offset
; Gets the offset from the origin of a position
(define (position-coords p)
  (cell-offset (position-cell p) (position-offset p)))

;;>
;;< Character functions
; data Character = Character Position Direction (Maybe Direction)
(define-struct character (position direction next-direction))

; Character
; Starting position of pacman
(define default-start (make-character (make-position (make-cell 14 23)
                                                     (make-offset half-cell half-cell))
                                      'west
                                      #f))
; Character
; Starting position of ghost
(define ghost-start (make-character (make-position (make-cell 14 11)
                                                     (make-offset half-cell half-cell))
                                      'west
                                      #f))

; Character Position -> Character
(define (character-set-position c p)
  (make-character p
                  (character-direction c)
                  (character-next-direction c)))

; Character Direction -> Character
(define (character-set-direction c d)
  (make-character (character-position c)
                  d
                  (character-next-direction c)))

; Character Direction -> Character
(define (character-set-next-direction c d)
  (make-character (character-position c)
                  (character-direction c)
                  d))

; Character -> Character
; Resets the character's next direction to #f
(define (clear-next-dir c)
  (character-set-next-direction c #f))

; Character -> Bool
; Returns true if the character is near the center of the cell
(define (at-center pos)
  (let ([o (position-offset pos)])
    (and (< (- half-cell 3) (offset-x o) (+ half-cell 3))
         (< (- half-cell 3) (offset-y o) (+ half-cell 3)))))

; Character Direction -> Character
; Returns true if the character would be able to move to the next cell in the
; given direction
(define (can-advance pos dir)
  (let* ([cell (cell-add (position-cell pos)
                         (cell-direction dir))]
         [maze-cell (grid-ref default-maze
                              (cell-i cell)
                              (cell-j cell))])
    (not maze-cell)))

; Character -> Character
; Returns true if the character has room to move in its current direction in its
; current cell
(define (can-move-in-cell c)
  (let* ([pos (character-position c)]
         [offset (position-offset pos)]
         [dir (character-direction c)]
         [x (offset-x offset)]
         [y (offset-y offset)])
    (cond
      [(symbol=? dir 'north) (> y half-cell)]
      [(symbol=? dir 'south) (< y half-cell)]
      [(symbol=? dir 'east) (< x half-cell)]
      [(symbol=? dir 'west) (> x half-cell)])))

; Character -> Bool
; Returns true if the character will be able to move this frame
(define (can-move c)
  (or (can-move-in-cell c)
      (can-advance (character-position c)
                   (character-direction c))))

; Character -> Character
; Re-places the character along the middle of the cell they are travelling in
(define (place-in-center c)
  (let* ([position (character-position c)]
         [offset (position-offset position)]
         [dir (character-direction c)])
    (character-set-position c
                            (make-position (position-cell position)
                                           (center-offset offset dir)))))

; Character -> Character
; If the character's next-direction is set and they approach an intersection
; that they can turn into, change their direction
(define (toggle-directions c)
  (if (and (at-center (character-position c))
           (not (false? (character-next-direction c)))
           (can-advance (character-position c)
                        (character-next-direction c)))
    (character-set-direction c (character-next-direction c))
    c))


; Character Int -> Character
; Move the character in their current direction at the given speed
(define (move-character c speed)
  (if (can-move c)
    (character-set-position
      c
      (position-move (character-position c)
                     (offset-scale speed
                                   (offset-direction (character-direction c)))))
    c))

; Character Image -> Drawing
; Draws the character with the given image
(define (draw-character c img)
  (let ([coord (position-coords (character-position c))])
    (draw-at/center img
                    (offset-x coord)
                    (offset-y coord))))

;;>
;;< Pacman functions

; Character -> Drawing
; Draws Pacman for the given character
(define (pacman-drawing c)
  (draw-character c (circle (* 1.8 half-cell) 'solid 'yellow)))

;;>
;;< Ghost functions

; Character -> Drawing
; Draws a ghost
(define (ghost-drawing c)
  (draw-character c (circle (* 1.3 half-cell) 'solid 'magenta)))


;;>
;;< GameState functions

; type Pills = Grid Bool

; data GameState = GameState Pills Character Character

(define-struct game-state (pills pacman ghost))

; GameState Character -> GameState
(define (game-state-set-pacman s pacman)
  (make-game-state (game-state-pills s)
                   pacman
                   (game-state-ghost s)))

; GameState Pills -> GameState
(define (game-state-set-pills s pills)
  (make-game-state pills
                   (game-state-pacman s)
                   (game-state-ghost s)))

; GameState Character -> GameState
(define (game-state-set-ghost s ghost)
  (make-game-state (game-state-pills s)
                   (game-state-pacman s)
                   ghost))

; Bool Int Int -> Drawing
; draws a single pilll at i, j
(define (pill-drawing pill i j)
  (if pill
    (draw-at/center (circle 2 'solid 'white)
                    (+ (* maze-cell-size i) half-cell)
                    (+ (* maze-cell-size j) half-cell))
    identity))

; Pills -> Drawing
; Draws all pills
(define (pills-drawing ps)
  (combine-drawings (grid-contents (grid-indexed-map ps
                                                     pill-drawing))))


; GameState -> Image
; Draws the game
(define (draw-game s)
  (apply-drawings rendered-maze
                  (list
                    (pills-drawing (game-state-pills s))
                    (ghost-drawing (game-state-ghost s))
                    (pacman-drawing (game-state-pacman s)))))

(define (character-movement c speed)
  (place-in-center
    (move-character
      (toggle-directions c) speed)))

; GameState -> GameState
; Moves Pacman
(define (pacman-movement s)
  (game-state-set-pacman s
                         (character-movement (game-state-pacman s)
                                             pacman-speed)))

; GameState -> GameSTate
; Moves the ghost
(define (ghost-movement s)
  (game-state-set-ghost s
                        (character-movement (game-state-ghost s)
                                            ghost-speed)))

; GameState -> GameState
; Makes Pacman eat the pill he is sitting on if he can
(define (pill-eating s)
  (let* ([pacman (game-state-pacman s)]
         [pos (character-position pacman)]
         [cell (position-cell pos)]
         [i (cell-i cell)]
         [j (cell-j cell)]
         [pills (game-state-pills s)])
    (if (and (at-center pos)
             (grid-ref pills i j))
      (game-state-set-pills s
                            (grid-set pills i j #f))
      s)))


; GameState -> GameState
; Compute the game state of the next frame
(define (tock s)
  (ghost-movement
    (pill-eating
      (pacman-movement s))))

; GameState Direction -> GameState
; Makes Pacman change direction if he can
(define (set-direction s dir)
  (let ([pacman (game-state-pacman s)])
    (game-state-set-pacman
      s
      (if (and (can-advance (character-position pacman) dir)
               (at-center (character-position pacman)))
        (character-set-direction (clear-next-dir pacman)
                                 dir)
        (character-set-next-direction pacman
                                      dir)))))

; GameState Key -> GameState
; Key handler function
(define (handle-key s k)
  (cond
    [(key=? k "up") (set-direction s 'north)]
    [(key=? k "down") (set-direction s 'south)]
    [(key=? k "left") (set-direction s 'west)]
    [(key=? k "right") (set-direction s 'east)]
    [else s]))
;;>

(big-bang (make-game-state (grid-map default-maze not)
                           default-start
                           ghost-start)
          [to-draw draw-game]
          [on-key handle-key]
          [on-tick tock])
