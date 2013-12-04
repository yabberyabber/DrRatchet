;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname grid-tools) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
;;grid-tools.rkt provides tools for creating and modifying a grid of boxes

(require "exporty.rkt")
(require "constants.rkt")
(provide create-grid)
(provide toggle-square)
(provide LOB)


; create-row takes an x and y and returns a list of 
; squares all with y-posn y and with x-posns ranging from 1 to x
; number number -> list-of-squares
(define (create-row x y)
  (cond [(equal? x 0) empty]
        [else (cons (make-sq-part SQR-SIZE 
                                  (make-posn (x-offset x)
                                             (y-offset y)) 
                                  false)
                    (create-row (- x 1) y))]))

(check-expect (create-row 2 1)
              (list (make-sq-part SQR-SIZE (make-posn (x-offset 2) (y-offset 1)) false)
                    (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false)))
(check-expect (create-row 3 2)
              (list (make-sq-part SQR-SIZE (make-posn (x-offset 3) (y-offset 2)) false)
                    (make-sq-part SQR-SIZE (make-posn (x-offset 2) (y-offset 2)) false)
                    (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 2)) false)))

; grid takes an x and y and returns a list of squares
; with y-posns ranging from 1 to y and x-posns ranging from 1 to x
; number number list-of-squares -> list-of-squares
(define (create-grid x y LOB)
  (cond [(equal? y 0) LOB]
        [else (create-grid x (- y 1)
                           (append LOB
                                   (create-row x y)))]))

(check-expect (create-grid 1 3 empty)
              (list (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 3)) false)
                    (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 2)) false)
                    (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false)))


; toggle-square takes an x and a y and
; a list-of-squares and returns the list of squares with
; the square at position x and y with the state flipped
; number number list-of-squares -> list-of-squares
(define (toggle-square x y LOS)
  (cond [(empty? LOS) empty]
        [else
         (cond [(and (equal? (x-offset x) (posn-x (sq-part-posn (first LOS))))
                     (equal? (y-offset y) (posn-y (sq-part-posn (first LOS)))))
                (cons (make-sq-part SQR-SIZE
                                    (sq-part-posn (first LOS))
                                    (not (sq-part-state (first LOS))))
                      (toggle-square x y (rest LOS)))]
               [else (cons (first LOS) (toggle-square x y (rest LOS)))])]))


; test constants
(define LOB (create-grid 2 2 empty))

(check-expect (toggle-square (- (x-offset 2) 1) (+ (y-offset 1) 1) LOB)
              (list (make-sq-part SQR-SIZE (make-posn (x-offset 2) (y-offset 2)) false)
                    (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 2)) false)
                    (make-sq-part SQR-SIZE (make-posn (x-offset 2) (y-offset 1)) false)
                    (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false)))