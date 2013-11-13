;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Final Project|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
(define WIDTH 400)
(define HEIGHT 400)
(define SQRS 3)
(define MT-SCN (empty-scene WIDTH HEIGHT))
(define SQR-SIZE (/ WIDTH 10))
(define X-PAD (/ WIDTH (* 2 SQRS)))


; number -> number
; set the y offset value for each square
(define (y-offset n)
  (- (* n (/ WIDTH SQRS)) (/ WIDTH (* 2 SQRS))))

(check-expect (y-offset 5) (- (* 5 (/ 400 3)) (/ WIDTH 6)))

; number -> number
; set the x offset value for each square
(define (x-offset n)
 (- (* n (/ HEIGHT SQRS)) X-PAD))

(check-expect (x-offset 2) (- (* 2 (/ 400 3)) X-PAD))


; a sq-part is (make-sq-part number posn boolean)
(define-struct sq-part (len posn state))


; a list-of-dims is one of:
; - empty, or
; (cons number list-of-dims)
; place square tiles on background
; list-of-dims number -> image
(define (sqr-placer lod)
  (cond
    [(empty? lod) MT-SCN]
    [else (place-image (square
                              (sq-part-len (first lod))
                              "solid"
                              "red")
                             (posn-x (sq-part-posn (first lod)))
                             (posn-y (sq-part-posn (first lod)))
                             (sqr-placer (rest lod)))]))

(check-expect (sqr-placer (cons (make-sq-part 5
                                              (make-posn 10 20)
                                              true)
                                empty))
              (place-image (square 5 "solid" "red")
                           10 20
                           MT-SCN))



;big bang
(big-bang (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false)
                  (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 2)) false)
                        (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 3)) false)
                              (cons (make-sq-part SQR-SIZE (make-posn (x-offset 2) (y-offset 1)) false)
                                    (cons (make-sq-part SQR-SIZE (make-posn (x-offset 2) (y-offset 2)) false)
                                          (cons (make-sq-part SQR-SIZE (make-posn (x-offset 2) (y-offset 3)) false)
                                                (cons (make-sq-part SQR-SIZE (make-posn (x-offset 3) (y-offset 1)) false)
                                                      (cons (make-sq-part SQR-SIZE (make-posn (x-offset 3) (y-offset 2)) false)
                                                            (cons (make-sq-part SQR-SIZE (make-posn (x-offset 3) (y-offset 3)) false) empty)))))))))
            [to-draw sqr-placer])