#lang racket/gui

; digit-construction things

(define phi 1.6180339887498948482)

(define (draw-zero dc x y s) ; the diamond
  (let ([ps (* phi s)])
    (send dc draw-line x (- y ps) (+ x s) y) ; north to east
    (send dc draw-line (+ x s) y x (+ y ps)) ; east to south
    (send dc draw-line x (+ y ps) (- x s) y) ; south to west
    (send dc draw-line (- x s) y x (- y ps)) ; west to north
    ))

(define (draw-one dc x y s) ; vertical central line
  (let ([ps (* phi s)])
    (send dc draw-line x (- y ps) x (+ y ps))))

(define (draw-two dc x y s) ; N figure
  (let ([hp (/ (* phi s) 2)]
        [hs (/ s 2)])
    (send dc draw-line (- x hs) (+ y hp) (- x hs) (- y hp)) ; southwest to northwest
    (send dc draw-line (- x hs) (- y hp) (+ x hs) (+ y hp)) ; northwest to southeast
    (send dc draw-line (+ x hs) (+ y hp) (+ x hs) (- y hp)) ; southeast to northeast
    ))

(define (draw-four dc x y s) ; horizontal central line
  (let ([ps (* phi s)])
    (send dc draw-line (+ x s) y (- x s) y)))
(define (draw-eight dc x y s) ; horizontal parallels
  (let ([hp (/ (* phi s) 2)]
        [hs (/ s 2)])
    (send dc draw-line (- x hs) (- y hp) (+ x hs) (- y hp)) ; northwest to northeast
    (send dc draw-line (- x hs) (+ y hp) (+ x hs) (+ y hp)) ; southwest to southeast
    ))

(define (draw-16 dc x y s) ; diagonal lower left
  (let ([hp (/ (* phi s) 2)]
        [hs (/ s 2)])
    (send dc draw-line (- x hs) (+ y hp) x y)))
(define (draw-32 dc x y s) ; diagonal upper right
  (let ([hp (/ (* phi s) 2)]
        [hs (/ s 2)])
    (send dc draw-line x y (+ x hs) (- y hp))))

; and put it all together...
(define (draw-digit d dc x y s)
  (draw-zero dc x y s)
  (when (odd? d)
    (draw-one dc x y s))
  (when (bitwise-bit-set? d 1)
    (draw-two dc x y s))
  (when (bitwise-bit-set? d 2)
    (draw-four dc x y s))
  (when (bitwise-bit-set? d 3)
    (draw-eight dc x y s))
  (when (bitwise-bit-set? d 4)
    (draw-16 dc x y s))
  (when (bitwise-bit-set? d 5)
    (draw-32 dc x y s)))

(define (draw-number n dc x y s)
  (send dc set-pen "black" 2 'solid)
  (draw-digit 0 dc x y s)
  (send dc set-pen "black" 1 'solid)
  (if (< n 64)
      (draw-digit n dc x y s)
      (let* ([d1 (modulo n 64)]
             [d2 (/ (- n d1) 64)])
        (error))))


; canvasing things
(define the-grid
  (build-vector 8 (λ (n) (build-vector 8 (λ (m) (+ (* n 8) m))))))
(define (draw-grid dc)
  (send dc set-smoothing 'smoothed)
  ; draw the grid
  ; first, get dimensions and place the center
  (let*-values ([(w h) (send dc get-size)]
                [(cx) (/ w 2)]
                [(cy) (/ h 2)]
                [(row-length) (vector-length (vector-ref the-grid 1))]
                [(column-length) (vector-length the-grid)]
                [(row-length-real) (+ row-length 1)]
                [(column-length-real) (* (+ (/ column-length 2) 1) phi)]
                [(row-scale) (/ w row-length-real)]
                [(column-scale) (/ h column-length-real)]
                [(true-scale) (min row-scale column-scale)]
                [(start-x) (- cx (/ (* row-length true-scale) 2))]
                [(start-y) (- cy (/ (* (- column-length 2) true-scale) 2))])
    (for([i (in-range column-length)])
      (for ([j (in-range row-length)])
        (draw-number
         (vector-ref (vector-ref the-grid i) j) dc
         (if (even? i)
             (+ start-x (* true-scale (+ j 1/2)))
             (+ start-x (* true-scale j)))
         (+ start-y (* true-scale i phi 1/2)) (* true-scale 1/2))
        ))
    ))


; windowing things
(define frame (new frame% [label "Tiles"] [height 600] [width 800]))

(new canvas% [parent frame]
     [paint-callback
      (λ (canvas dc)
        (draw-grid dc))])

(send frame show #t)