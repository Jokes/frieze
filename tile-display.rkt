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
  (let ([hp (/ (* phi s) 2)])
    (send dc draw-line (- x hp) (+ y hp) (- x hp) (- y hp)) ; southwest to northwest
    (send dc draw-line (- x hp) (- y hp) (+ x hp) (+ y hp)) ; northwest to southeast
    (send dc draw-line (+ x hp) (+ y hp) (+ x hp) (- y hp)) ; southeast to northeast
    ))

(define (draw-four dc x y s) ; horizontal central line
  (let ([ps (* phi s)])
  (send dc draw-line (+ x s) y) (- x s) y))
(define (draw-eight dc x y s) ; horizontal parallels
  (let ([hp (/ (* phi s) 2)])
  (send dc draw-line (- x hp) (- y hp) (+ x hp) (- y hp)) ; northwest to northeast
  (send dc draw-line (- x hp) (+ y hp) (+ x hp) (+ y hp)) ; southwest to southeast
    ))

(define (draw-16 dc x y s) ; diagonal lower left
  (let ([hp (/ (* phi s) 2)])
  (send dc draw-line (- x hp) (+ y hp) x y)))
(define (draw-32 dc x y s) ; diagonal upper right
  (let ([hp (/ (* phi s) 2)])
  (send dc draw-line x y (+ x hp) (- y hp))))

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
  (if (< n 64)
      (draw-digit n dc x y s)
      (error)))


; canvasing things
(define the-grid
  (build-vector 8 (λ (n) (build-vector 8 (λ (m) (+ (* n 8) m))))))
(define (draw-grid dc)
  ; draw the grid
  (error))


; windowing things
(define frame (new frame% [label "Tiles"]))

(new canvas% [parent frame]
     [paint-callback
      (λ (canvas dc)
        (draw-grid dc))])

(send frame show #t)