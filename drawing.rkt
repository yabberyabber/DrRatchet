;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname drawing) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
(require "exporty.rkt")
(require rsound)
(require 2htdp/universe)
(require 2htdp/image)
(require "constants.rkt")
(require "world.rkt")
(require "grid-tools.rkt")

(provide (all-defined-out))

; Menu Screen
; world -> world
; create the menu screen
(define (draw-menu w)
  (place-image INSTRUCTIONS (/ W-WIDTH 2) (/ W-HEIGHT 2)
               MT-SCN))


; number number number -> number
; change the x-position of the sliding line
(define (line-posn t tempo offset)
  (* (/ WIDTH (s (measure-length tempo)))
                   (modulo (round (- (+ t offset) SOUND-BUFFER (/ (s (measure-length tempo)) SQRS)))
                           (round (s (measure-length tempo))))))

(check-expect (line-posn 21000 160 0) (* (/ WIDTH (s (measure-length 160)))
                                         (modulo (round (- (+ 21000 0) SOUND-BUFFER (/ (s (measure-length 160)) SQRS)))
                                                 (round (s (measure-length 160))))))

; world -> image
; draw the menu or the world
(define (draw-world w)
  (local [(define cur-fr (pstream-current-frame ps))]
  (if (world-menu w)
      (draw-menu w)
      (add-line (place-image (text (number->string (world-tempo w)) 28 "red") 150 485
                (place-image (text (number->string (world-offset w)) 28 "red") 450 485
                (sqr-placer  (world-boxes w))))
                (line-posn (pstream-current-frame ps) (world-tempo w) (world-offset w))
                0
                (line-posn (pstream-current-frame ps) (world-tempo w) (world-offset w))
            HEIGHT
            "red"))))

#;(check-expect (draw-world (make-world (cons (make-sq-part 5
                                              (make-posn 10 20)
                                              true)
                                empty) 0 false 0 DEFAULT-TEMPO DEFAULT-OFFSET true))
              (add-line (place-image (text "120" 28 "red") 150 485
                        (place-image (text "0" 28 "red") 450 485
                (place-image BUTTON-GREEN
                           10 20
                              (place-image BACKGROUND (/ W-WIDTH 2) (/ W-HEIGHT 2 ) MT-SCN))))
                        (line-posn (pstream-current-frame ps) 160 0)
                        0
                        (line-posn (pstream-current-frame ps) 160 0)
                        HEIGHT
                        "red"))

; a list-of-dims is one of:
; - empty, or
; (cons number list-of-dims)
; place square tiles and labels on background
; list-of-dims number -> image
(define (sqr-placer lod)
  (cond
    [(empty? lod) BACKGROUND]
    [else (place-image (if (sq-part-state (first lod))
                           (mapRowtoColor (y-pt->y-gd (posn-y (sq-part-posn (first lod)))))
                           BUTTON-OFF)
                       (posn-x (sq-part-posn (first lod)))
                       (posn-y (sq-part-posn (first lod)))
                       (sqr-placer (rest lod)))]))



; number -> sound
; Maps the row that a button is in
; to the color it should change to

(define (mapRowtoColor row)
  (cond [(= row 1) BUTTON-GREEN]
        [(= row 2) BUTTON-BLUE]
        [(= row 3) BUTTON-YELLOW]
        [(= row 4) BUTTON-BROWN]
        [(= row 5) BUTTON-LIGHTBLUE]
        [(= row 6) BUTTON-RED]
        [(= row 7) BUTTON-LIGHTGREEN]
        [(= row 8) BUTTON-PINK]
        [else BUTTON-OFF]
        )
  )

(check-expect (mapRowtoColor 1) BUTTON-GREEN)
(check-expect (mapRowtoColor 0) BUTTON-OFF)
