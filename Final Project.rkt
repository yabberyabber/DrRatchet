;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Final Project|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
(require rsound)
(require 2htdp/universe)
(require "get-file.rkt")

;;;;;
;
; CONSTANTS
;
;;;;;

(require "constants.rkt")

(require "world.rkt")
(require "grid-tools.rkt")

;;;;;
;
; Draw World
;
;;;;;

(require "drawing.rkt") ;provides the draw function

;;;;;
;
; Mouse Handler
;
;;;;

(require "mouse-handling.rkt")  ;provides the me-h (mouse handling) function

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
             (if (and (world-sp-b w) (not (world-menu w)))
                 (local
                   [(define userfile (get-file))]
                   (cond [(boolean? userfile) w]
                         [else 
                          (both (pstream-queue ps (rs-scale 0.2 (rs-read userfile))
                                               (pstream-current-frame ps))
                                (make-world (world-boxes w)
                                            (world-time w)
                                            (world-menu w)
                                            (world-next-play-time w)
                                            (world-tempo w)
                                            (world-offset w)
                                            false))]))
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

(require "sounding.rkt")  ;provides the tick-handler function


;;;;;
;
; BIG BANG
;
;;;;;

(big-bang (make-world (create-grid SQRS SQRS empty) 0 true 0 DEFAULT-TEMPO DEFAULT-OFFSET true)
          [to-draw draw]
          [on-mouse me-h]
          [on-key ke-h]
          [on-tick tick-handler])

; stop the music after big-bang returns
(stop)