#lang racket

; okay what are my goals here

; step one: a frieze pattern generator, given either a lightning bolt or a top row
; step two: a number grid renderer, given a number-to-tile converter and a number grid



; let's start with step one

(struct Frieze (grid height) #:mutable #:transparent)

(define (Frieze-width f) (+ (Frieze-height f) 3))

(define (apply-and l)
  (if (empty? l)
      #t
      (and (car l) (apply-and (cdr l)))))

(define (f-complete? f)
  (apply-and (vector->list (vector-map (λ (r) (apply-and (vector->list r))) (Frieze-grid f)))))

(define (f-ref f x y)
  (if (or
       (< y -1)
       (> y (Frieze-height f)))
      #f
      (if (or (equal? y -1) (equal? y (Frieze-height f)))
          1
          (vector-ref (vector-ref (Frieze-grid f) y)
                      (modulo x (Frieze-width f))))))

(define (f-set! f x y v)
  (vector-set! (vector-ref (Frieze-grid f) y) x v))

(define (f-copy f)
  (Frieze
   (vector-map vector-copy (Frieze-grid f))
   (Frieze-height f)))

(define (north-of f x y)
  (f-ref f x (- y 2)))
(define (northeast-of f x y)
  (f-ref f (if (even? y) x (add1 x)) (sub1 y)))
(define (east-of f x y)
  (f-ref f (add1 x) y))
(define (southeast-of f x y)
  (f-ref f (if (even? y) x (add1 x)) (add1 y)))
(define (south-of f x y)
  (f-ref f x (+ 2 y)))
(define (southwest-of f x y)
  (f-ref f (if (even? y) (sub1 x) x) (add1 y)))
(define (west-of f x y)
  (f-ref f (sub1 x) y))
(define (northwest-of f x y)
  (f-ref f (if (even? y) (sub1 x) x) (sub1 y)))
      

(define (f-iterate f1)
  ; go through every cell checking if it needs filling, if it's fillable, filling it if so, moving on
  (let ([f (f-copy f1)])
    (for ([i (in-range (Frieze-width f))])
      (for ([j (in-range (Frieze-height f))])
        (unless (f-ref f i j)
          (let ([nn (north-of f i j)]
                [ne (northeast-of f i j)]
                [ee (east-of f i j)]
                [se (southeast-of f i j)]
                [ss (south-of f i j)]
                [sw (southwest-of f i j)]
                [ww (west-of f i j)]
                [nw (northwest-of f i j)])
            (f-set! f i j
                    (cond
                      [(and nw nn ne) (/ (- (* nw ne) 1) nn)] ; we are SOUTH
                      [(and nw ww sw) (/ (+ (* nw sw) 1) ww)] ; we are EAST
                      [(and se ee ne) (/ (+ (* ne se) 1) ee)] ; we are WEST
                      [(and se ss sw) (/ (- (* sw se) 1) ss)] ; we are NORTH
                      [else #f] ; we AREN'T
                      )))))
      f)
    f))

(define (fill-frieze f)
  (displayln f)
  (if (f-complete? f)
      f
      (let ([f2 (f-iterate f)])
        (displayln f)
        (displayln f2)
        (if (equal? f f2)
            #f ; something got fucked up here
            (fill-frieze f2)))))

(define (from-top tr)
  ; bookkeeping
  (let ([top-row (if (list? tr) (list->vector tr) tr)]
        [height (- (length tr) 3)])
    ; generate a frieze
    (fill-frieze
     (Frieze
      (build-vector
       height
       (λ (n)
         (if (zero? n)
             top-row
             (make-vector (length tr) #f))))
      height))))