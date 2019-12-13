#lang racket/gui

; digit-construction things

(define phi 1.6180339887498948482)

(define (draw-zero dc x y s h)
  (error))

(define (draw-one dc x y s h)
  (error))
(define (draw-two dc x y s h)
  (error))

(define (draw-four dc x y s h)
  (error))
(define (draw-eight dc x y s h)
  (error))

(define (draw-16 dc x y s h)
  (error))
(define (draw-32 dc x y s h)
  (error))

; check for things

(define (two? d) ; 2 6 A E 3 7 B F
  (let ([dm (modulo d 16)])
    (error)))
(define (four? d) ; 4 5 6 7 C D E F
  (let ([dm (modulo d 16)])
    (error)))
(define (eight? d) ; 8 9 A B C D E F
  (let ([dm (modulo d 16)])
    (error)))

(define (16? d) ; 16-31 incl
  (and (>= d 16) (<= d 31)))
(define (32? d) ; 32-47 incl
  (and (>= d 32) (<= d 47)))
(define (48? d) ; 48-63 incl
  (and (>= d 48) (<= d 63)))

; and put it all together...
(define (draw-digit d dc x y s)
  (let ([h (48? d)])
    (when (odd? d)
      (draw-one dc x y s h))
    (when (two? d)
      (draw-two dc x y s h))
    (when (four? d)
      (draw-four dc x y s h))
    (when (eight? d)
      (draw-eight dc x y s h))
    (when (16? d)
      (draw-16 dc x y s h))
    (when (or (32? d) h)
      (draw-32 dc x y s))))

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