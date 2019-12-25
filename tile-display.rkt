#lang racket/gui

(require "frieze-gen.rkt")

; digit-construction things

(define phi 1.6180339887498948482)

(define palette-lookup
  (make-hash
   (list
    (list "Ice" (make-color 236 245 255) (make-color 213 226 252) (make-color 189 206 247)
          (make-color 164 187 243) (make-color 148 159 225) (make-color 134 133 207)
          (make-color 119 109 188))
    (list "Sunset" (make-color 255 253 228) (make-color 249 230 192) (make-color 237 186 146)
          (make-color 220 134 97) (make-color 201 110 103) (make-color 179 88 110)
          (make-color 154 65 118)))))
(define palette-choices (sort (hash-keys palette-lookup) string<?))

(define palette
  (hash-ref palette-lookup (first palette-choices)))
(define (get-fill-col n)
  (list-ref palette (modulo n (length palette))))

(define (zero-path x y s)
  (let ([p (new dc-path%)]
        [ps (* phi s)])
    (send p move-to x (- y ps))
    (send p line-to (+ x s) y)
    (send p line-to x (+ y ps))
    (send p line-to (- x s) y)
    (send p close)
    p))

(define (draw-zero dc x y s) ; the diamond
  (send dc draw-path (zero-path x y s)))

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

(define (clip-digit dc x y s)
  (let-values ([(r) (new region% [dc dc])]
               [(r2) (new region% [dc dc])]
               [(w h) (send dc get-size)])
    (send r set-rectangle 0 0 w h)
    (send r2 set-path (zero-path x y s))
    (send r subtract r2)
    r))

(define (draw-number n dc x y s)
  (draw-digit 0 dc x y s)
  (send dc set-pen "black" 1 'solid)
  (if (< n 64)
      (draw-digit n dc x y s)
      (let* ([d1 (modulo n 64)]
             [d2 (/ (- n d1) 64)])
        (draw-number d2 dc x y (/ s 2))
        (send dc set-clipping-region (clip-digit dc x y (/ s 2)))
        (draw-digit d1 dc x y s)
        (send dc set-clipping-region #f))))

(define (draw-number-base n dc x y s)
  (send dc set-brush (get-fill-col n) 'solid)
  (send dc draw-path (zero-path x y s))
  (send dc set-brush "black" 'transparent)
  (draw-number n dc x y s))

; canvasing things
(define all-digits
  (build-vector 8 (λ (n) (build-vector 8 (λ (m) (+ (* n 8) m))))))
(define the-grid all-digits)

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
                [(start-x) (- cx (* (- row-length 1/2) true-scale 1/2))]
                [(start-y) (- cy (* (- column-length 1) phi true-scale 1/4))])
    (send dc draw-rectangle 0 0 w h)
    (for([i (in-range column-length)])
      (for ([j (in-range row-length)])
        (send dc set-pen "black" 2 'solid)
        (draw-number-base
         (vector-ref (vector-ref the-grid i) j) dc
         (if (odd? i)
             (+ start-x (* true-scale (+ j 1/2)))
             (+ start-x (* true-scale j)))
         (+ start-y (* true-scale i phi 1/2)) (* true-scale 1/2))
        ))
    ))

; export
(define (export-grid)
  (let*-values ([(w h) (send (send main-canvas get-dc) get-size)]
                [(bmp) (make-bitmap w h)]
                [(bdc) (new bitmap-dc% [bitmap bmp])])
    (draw-grid bdc)
    (send bmp save-file
          (string-append
           (string-join (map number->string (vector->list (vector-ref the-grid 1))))
           ".png") 'png)))

; windowing things
(define frame (new frame% [label "Tiles"] [height 600] [width 800]))
(define canvas-p (new vertical-panel% [parent frame] [alignment '(center center)]))
(define menu-p (new horizontal-panel% [parent canvas-p] [alignment '(center center)]
                    [stretchable-height #f]))

(define digits-b 
  (new button% [parent menu-p] [label "Display All Digits"]
       [callback (λ (b e) (set! the-grid all-digits) (send main-canvas refresh))]))

(define input-t
  (new text-field% [parent menu-p] [label ""]))

(define input-b
  (new button% [parent menu-p] [label "Generate Frieze"]
       [callback
        (λ (b e)
          (set! the-grid
                (let ([spl (string-split (send input-t get-value))])
                  (if (string->number (first spl))
                      (Frieze-grid (from-top (map string->number spl)))
                      (Frieze-grid (from-bolt (map (λ (s) (equal? s "#t")) spl))))))
          (send main-canvas refresh))]))

(define pal-drop
  (new choice% [label "Palette: "]
       [parent menu-p]
       [callback
        (λ (c e)
          (set! palette (hash-ref palette-lookup (list-ref palette-choices (send c get-selection))))
          (send main-canvas refresh))]
       [choices palette-choices]))

(define export-b 
  (new button% [parent menu-p] [label "Export"] [callback (λ (b e) (export-grid))]))

(define main-canvas
  (new canvas% [parent canvas-p]
       [paint-callback
        (λ (canvas dc)
          (draw-grid dc))]))

(send frame show #t)