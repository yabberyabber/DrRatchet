(require 2htdp/image)
(define WIDTH 400)
(define HEIGHT WIDTH)

;numsquares must be a square of a whole number
;(define NUMSQUARES 64)
;(define ROWLENGTH (sqrt NUMSQUARES))
(define ROWLENGTH 8)
(define BETWEENSQUARES (round (/ WIDTH (+ 1 ROWLENGTH))))
(define padding (round (/ BETWEENSQUARES 2)))

(define (both a b) b)
(define (drawBoxes x y max)
  (if (= max x)
      (if (= max y)
          (place-image
           (square 10 "solid" "red")
           (+ padding (* x BETWEENSQUARES))
           (+ padding (* y BETWEENSQUARES))
           (empty-scene WIDTH HEIGHT))
          (place-image
           (square 10 "solid" "red")
           (+ padding (* x BETWEENSQUARES))
           (+ padding (* y BETWEENSQUARES))
           (drawBoxes 0 (+ y 1) max)
           )
          )
      (place-image
       (square 10 "solid" "red")
       (+ padding (* x BETWEENSQUARES))
       (+ padding (* y BETWEENSQUARES))
       (drawBoxes (+ x 1) y max)
       )
      )
  )
(drawBoxes 0 0 7)