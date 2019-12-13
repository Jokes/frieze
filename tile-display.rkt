#lang racket/gui

; digit-construction things

(define phi 1.6180339887498948482)

(define (draw-zero dc x y s) ; the diamond
  (error))

(define (draw-one dc x y s) ; vertical central line
  (error))
(define (draw-two dc x y s) ; N figure
  (error))

(define (draw-four dc x y h) ; horizontal central line
  (error))
(define (draw-eight dc x y s) ; horizontal parallels
  (error))

(define (draw-16 dc x y s) ; diagonal lower left
  (error))
(define (draw-32 dc x y s) ; diagonal upper right
  (error))

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
(define (draw-grid dc)
  (error))


; windowing things
(define frame (new frame% [label "Tiles"]))

(new canvas% [parent frame]
     [paint-callback
      (λ (canvas dc)
        (draw-grid dc))])

(send frame show #t)