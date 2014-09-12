#lang htdp/isl+

(require 2htdp/image)
(require 2htdp/universe)

; K-combinator
; X -> Y -> X
(define (constantly c)
  (lambda (x) c))

; Int X -> [X]
(define (repeat n x)
  (build-list n (constantly x)))

;; Basic grid (2d-array) abstractions

; data Grid X where
;   make-grid : Int [X] -> Grid X
;               {exists n:Int (length contents) = (* n width)}
(define-struct grid (width contents))

(define example-grid (make-grid 3 '(1 1 1 1 1 1 1 1 1)))

; Grid X -> Int
(define (grid-height g)
  (/ (length (grid-contents g))
     (grid-width g)))

; (Grid X) Int Int -> X
; 0 <= i < (grid-width g)
; 0 <= j < (grid-height g)
(define (grid-ref g i j)
  (list-ref (grid-contents g)
            (+ (* (grid-width g) j)
               i)))

; (Grid X) (X -> Y) -> Grid Y
(define (grid-map g f)
  (make-grid (grid-width g)
             (map f (grid-contents g))))

; (Grid Y) (X Y -> X) X -> X
(define (grid-fold g f x0)
  (foldl f x0 (grid-contents g)))

; (Grid X) (X Int Int -> Y) -> Grid Y
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

;; Drawing abstractions

; type Drawing = Image -> Image

; X [(X -> X)] -> X
(define (apply-each x0 fs)
  (foldl (lambda (f x) (f x)) x0 fs))

; Image [Drawing] -> Image
(define apply-drawings apply-each)

(define (draw-at img x y)
  (lambda (i0)
    (place-image/align img
                       x y
                       "left" "top"
                       i0)))

(define (draw-at/center img x y)
  (lambda (i0)
    (place-image img
                 x y
                 i0)))

(define maze-image (bitmap/file "pacman-maze.png"))

(define default-maze (make-grid (image-width maze-image)
                                (map (lambda (c) (equal? c (make-color 255 255 255)))
                                     (image->color-list maze-image))))

(define maze-width (grid-width default-maze))
(define maze-height (grid-height default-maze))


(define maze-cell-size 16)

(define maze-width-pixels (* maze-width maze-cell-size))
(define maze-height-pixels (* maze-height maze-cell-size))

(define half-cell (/ maze-cell-size 2))
(define maze-cell-color (make-color 0 0 60))

(define (draw-at-cell img i j)
  (draw-at img
           (* i maze-cell-size)
           (* j maze-cell-size)))

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

(define-struct cell (i j))

(define cell-zero (make-cell 0 0))

(define (cell-add c1 c2)
  (make-cell (remainder (+ (cell-i c1) (cell-i c2) maze-width) maze-width)
             (remainder (+ (cell-j c1) (cell-j c2) maze-height) maze-height)))

(define (cell-wrap c)
  (cell-add c cell-zero))

(define (cell-direction dir)
  (cond [(symbol=? dir 'north) (make-cell 0 -1)]
        [(symbol=? dir 'south) (make-cell 0 1)]
        [(symbol=? dir 'east) (make-cell 1 0 )]
        [(symbol=? dir 'west) (make-cell -1 0)]))

(define-struct offset (x y))

(define offset-zero (make-offset 0 0))

(define (offset-add o1 o2)
  (make-offset (remainder (+ (offset-x o1) (offset-x o2) maze-width-pixels)
                          maze-width-pixels)
               (remainder (+ (offset-y o1) (offset-y o2) maze-height-pixels)
                          maze-height-pixels))) 

(define (offset-wrap o)
  (offset-add o offset-zero))

(define (offset-scale s o)
  (offset-wrap (make-offset (* s (offset-x o))
                            (* s (offset-y o)))))

(define (offset-sub o1 o2)
  (offset-wrap (make-offset (- (offset-x o1) (offset-x o2))
                            (- (offset-y o1) (offset-y o2)))))

(define (cell->offset c)
  (offset-wrap (make-offset (* maze-cell-size (cell-i c))
                            (* maze-cell-size (cell-j c)))))

(define (offset->cell o)
  (cell-wrap (make-cell (floor (/ (offset-x o) maze-cell-size))
                        (floor (/ (offset-y o) maze-cell-size)))))


(define (cell-offset c o)
  (offset-add (cell->offset c)
                           o))

(define (offset-direction dir)
  (cond
    [(symbol=? dir 'north) (make-offset 0 -1)]
    [(symbol=? dir 'south) (make-offset 0 1)]
    [(symbol=? dir 'east) (make-offset 1 0)]
    [(symbol=? dir 'west) (make-offset -1 0)]))

(define (center-offset o dir)
  (cond
    [(symbol=? dir 'north) (make-offset half-cell (offset-y o))]
    [(symbol=? dir 'south) (make-offset half-cell (offset-y o))]
    [(symbol=? dir 'east) (make-offset (offset-x o) half-cell)]
    [(symbol=? dir 'west) (make-offset (offset-x o) half-cell)]))

(define-struct position (cell offset))

(define (reconcile-position p)
  (let* ([total     (cell-offset (position-cell p)
                                 (position-offset p))]
         [real-cell (offset->cell total)]
         [new-offset (offset-sub total (cell->offset real-cell))])
    (make-position real-cell new-offset)))


(define (position-move p o)
  (reconcile-position
    (make-position (position-cell p)
                   (offset-add (position-offset p)
                               o))))

(define (position-coords p)
  (cell-offset (position-cell p) (position-offset p)))

(define-struct character (position direction next-direction))

(define (character-set-position c p)
  (make-character p
                  (character-direction c)
                  (character-next-direction c)))

(define (character-set-direction c d)
  (make-character (character-position c)
                  d
                  (character-next-direction c)))

(define (character-set-next-direction c d)
  (make-character (character-position c)
                  (character-direction c)
                  d))

(define (clear-next-dir c)
  (character-set-next-direction c #f))

(define pacman-speed 3)

(define default-start (make-character (make-position (make-cell 14 23)
                                                     (make-offset half-cell half-cell))
                                      'west
                                      #f))

(define (at-center pos)
  (let ([o (position-offset pos)])
    (and (< (- half-cell 3) (offset-x o) (+ half-cell 3))
         (< (- half-cell 3) (offset-y o) (+ half-cell 3)))))

(define (can-advance pos dir)
  (let* ([cell (cell-add (position-cell pos)
                         (cell-direction dir))]
         [maze-cell (grid-ref default-maze
                              (cell-i cell)
                              (cell-j cell))])
    (not maze-cell)))

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

(define (can-move c)
  (or (can-move-in-cell c)
      (can-advance (character-position c)
                   (character-direction c))))

(define (place-in-center c)
  (let* ([position (character-position c)]
         [offset (position-offset position)]
         [dir (character-direction c)])
    (character-set-position c
                            (make-position (position-cell position)
                                           (center-offset offset dir)))))

(define (toggle-directions c)
  (if (and (at-center (character-position c))
           (not (false? (character-next-direction c)))
           (can-advance (character-position c)
                        (character-next-direction c)))
    (character-set-direction c (character-next-direction c))
    c))


(define (move-character c speed)
  (if (can-move c)
    (character-set-position
      c
      (position-move (character-position c)
                     (offset-scale speed
                                   (offset-direction (character-direction c)))))
    c))

(define (pacman-drawing c)
  (let ([coord (position-coords (character-position c))])
    (draw-at/center (circle (* 1.8 half-cell) 'solid 'yellow)
                    (offset-x coord)
                    (offset-y coord))))

(define-struct game-state (pills pacman))

(define (game-state-set-pacman s pacman)
  (make-game-state (game-state-pills s)
                   pacman))

(define (game-state-set-pills s pills)
  (make-game-state pills
                   (game-state-pacman s)))


(define (draw-game s)
  (apply-drawings rendered-maze
                  (list (pacman-drawing (game-state-pacman s)))))

(define (tock s)
  (let ([pacman (game-state-pacman s)])
    (game-state-set-pacman s
                           (place-in-center
                             (move-character
                               (toggle-directions pacman) pacman-speed)))))

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


(define (handle-key s k)
  (cond
    [(key=? k "up") (set-direction s 'north)]
    [(key=? k "down") (set-direction s 'south)]
    [(key=? k "left") (set-direction s 'west)]
    [(key=? k "right") (set-direction s 'east)]
    [else s]))


(define (show img)
  (animate (constantly img)))

(big-bang (make-game-state default-maze default-start)
          [to-draw draw-game]
          [on-key handle-key]
          [on-tick tock])
