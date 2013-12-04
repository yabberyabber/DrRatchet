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

(require "grid-tools.rkt")

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

(check-expect (me-h (make-world LOB 0 false 0 DEFAULT-TEMPO DEFAULT-OFFSET false) (x-offset 1) (y-offset 2)  "button-down")
              (make-world (cons (make-sq-part SQR-SIZE (make-posn (x-offset 2) (y-offset 1)) false)
                    (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false)
                          (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 3)) false)
                                (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 2)) true)
                                      (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false) empty))))) 0 false 0 DEFAULT-TEMPO DEFAULT-OFFSET false))
(check-expect (me-h (make-world LOB 0 false 0 DEFAULT-TEMPO DEFAULT-OFFSET false) (- (x-offset 2) 3) (+ (y-offset 1) 2) "button-down")
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
