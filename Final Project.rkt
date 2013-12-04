;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Final Project|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
(require rsound)
(require 2htdp/universe)
(require 2htdp/image)

;;;;;
;
; CONSTANTS
;
;;;;;

(require "constants.rkt")
(require "world.rkt")


(require "drawing.rkt")


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
              (cons (make-sq-part SQR-SIZE (make-posn (x-offset 2) (y-offset 1)) false)
                    (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false) empty)))
(check-expect (create-row 3 2)
              (cons (make-sq-part SQR-SIZE (make-posn (x-offset 3) (y-offset 2)) false)
                    (cons (make-sq-part SQR-SIZE (make-posn (x-offset 2) (y-offset 2)) false)
                          (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 2)) false) empty))))

; grid takes an x and y and returns a list of squares
; with y-posns ranging from 1 to y and x-posns ranging from 1 to x
; number number list-of-squares -> list-of-squares
(define (create-grid x y LOB)
  (cond [(equal? y 0) LOB]
        [else (create-grid x (- y 1)
                           (append LOB
                                   (create-row x y)))]))

(check-expect (create-grid 1 3 (create-row 2 1))
              (cons (make-sq-part SQR-SIZE (make-posn (x-offset 2) (y-offset 1)) false)
                    (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false)
                          (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 3)) false)
                                (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 2)) false)
                                      (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false) empty))))))


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
(define LOB-EX (make-world (cons (make-sq-part SQR-SIZE (make-posn (x-offset 2) (y-offset 1)) false)
                     (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false)
                           (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 3)) false)
                                 (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 2)) false)
                                       (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false) empty))))) 0 false 0 DEFAULT-TEMPO DEFAULT-OFFSET false))

(check-expect (toggle-square (- (x-offset 2) 1) (+ (y-offset 1) 1) (world-boxes LOB-EX))
              (cons (make-sq-part SQR-SIZE (make-posn (x-offset 2) (y-offset 1)) false)
                    (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false)
                          (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 3)) false)
                                (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 2)) false)
                                      (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false) empty))))))

;;;;;
;
; Mouse Handler
;
;;;;

; mouse handler handles mouse events.
; Depending on where the user clicked, toggles a square.
; world number number string -> world
(define (me-h w x y event)
  (if (world-menu w)
      (cond [(equal? event "button-down")
             (make-world
              (world-boxes w)
              (world-time w)
              false
              (world-next-play-time w)
              (world-tempo w)
              (world-offset w)
              (world-sp-b w))]
            [else w])
      (cond [(equal? event "button-down")
         (cond
           [(or (or (negative? (y-pt->y-gd y)) (zero? (y-pt->y-gd y)))
                (negative? (x-pt->x-gd x))) w]
           [else (make-world
                  (toggle-square (x-pt->x-gd x) (y-pt->y-gd y) (world-boxes w))
                  (world-time w)
                  (world-menu w)
                  (world-next-play-time w)
                  (world-tempo w)
                  (world-offset w)
                  (world-sp-b w))])]
            [else w])))

(check-expect (me-h LOB-EX (x-offset 1) (y-offset 2)  "button-down")
              (make-world (cons (make-sq-part SQR-SIZE (make-posn (x-offset 2) (y-offset 1)) false)
                    (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false)
                          (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 3)) false)
                                (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 2)) true)
                                      (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false) empty))))) 0 false 0 DEFAULT-TEMPO DEFAULT-OFFSET false))
(check-expect (me-h LOB-EX (- (x-offset 2) 3) (+ (y-offset 1) 2) "button-down")
              (make-world (cons (make-sq-part SQR-SIZE (make-posn (x-offset 2) (y-offset 1)) true)
                    (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false)
                          (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 3)) false)
                                (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 2)) false)
                                      (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false) empty))))) 0 false 0 DEFAULT-TEMPO DEFAULT-OFFSET false))

;;;;;
;
; Key Handler
;
;;;;


; world string -> world
; detect key presses to allow user to return
; to menu screen
(define (ke-h w event)
  (if (not (world-menu w))
      (cond [(equal? event "escape")
             (make-world (world-boxes w)
                         (world-time w)
                         true
                         (world-next-play-time w)
                         (world-tempo w)
                         (world-offset w)
                         (world-sp-b w))]
            [(equal? event "\r")
             (make-world (create-grid SQRS SQRS empty)
                         (world-time w)
                         false
                         (world-next-play-time w)
                         (world-tempo w)
                         (world-offset w)
                         (world-sp-b w))]
            [(equal? event " ")
             (if (and (and (world-sp-b w) (> (rs-frames USERWAV) 5)) (not (world-menu w)))
                 (both (pstream-queue ps USERWAV
                                      (pstream-current-frame ps))
                       (make-world (world-boxes w)
                                   (world-time w)
                                   (world-menu w)
                                   (world-next-play-time w)
                                   (world-tempo w)
                                   (world-offset w)
                                   false))
                 w)]
            [(equal? event "up")    (world-increment-tempo w)]
            [(equal? event "down")  (world-decrement-tempo w)]
            [(equal? event "left")  (world-decrement-offset w)]
            [(equal? event "right") (world-increment-offset w)]
            [else w])
      w))


;;;;;
;
; ON TICK
;
;;;;;

; number -> number
; determine the y-grid from a position
(define (y-grid y)
  (* y (/ HEIGHT SQRS)))

; number -> number
; determine the x-grid from a position
(define (x-grid x)
  (* x (/ WIDTH SQRS)))

(require "sounding.rkt")


;;;;;
;
; BIG BANG
;
;;;;;

(big-bang (make-world (create-grid SQRS SQRS empty) 0 true 0 DEFAULT-TEMPO DEFAULT-OFFSET true)
          [to-draw draw-world]
          [on-mouse me-h]
          [on-key ke-h]
          [on-tick tick-handler])
