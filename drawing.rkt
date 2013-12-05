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

(provide draw)

; world -> image
; calls either draw-menu or draw-world depending on what state the world is in
(define (draw w)
  (if (world-menu w)
      (draw-menu w)
      (draw-world w (pstream-current-frame ps))))

; Menu Screen
; world -> world
; create the menu screen
(define (draw-menu w)
  (place-image INSTRUCTIONS (/ W-WIDTH 2) (/ W-HEIGHT 2)
               MT-SCN))

; world number -> image
; Draw the world
(define (draw-world w current-frame)
  (add-line (place-image (text (number->string (world-tempo w)) 28 "red") 150 485
                         (place-image (text (number->string (/ (world-offset w) 44100)) 28 "red") 450 485
                                      (sqr-placer  (world-boxes w))))
            (line-posn current-frame (world-tempo w) (world-offset w))
            0
            (line-posn current-frame (world-tempo w) (world-offset w))
            HEIGHT
            "red"))

(check-expect (draw-world (make-world (list (make-sq-part 5 (make-posn 10 20) true)) 
                                      0 false 0 DEFAULT-TEMPO DEFAULT-OFFSET true) 
                          18)
              (add-line (place-image (text "128" 28 "red") 150 485
                                     (place-image (text "0" 28 "red") 450 485
                                                  (sqr-placer (list (make-sq-part 5 (make-posn 10 20) true)))))
                        (line-posn 18 DEFAULT-TEMPO DEFAULT-OFFSET)
                        0
                        (line-posn 18 DEFAULT-TEMPO DEFAULT-OFFSET)
                        HEIGHT
                        "red"))


; number number number -> number
; change the x-position of the sliding line
(define (line-posn t tempo offset)
  (* (/ WIDTH (s (measure-length tempo)))
                   (modulo (round (- (- t offset) SOUND-BUFFER (/ (s (measure-length tempo)) SQRS)))
                           (round (s (measure-length tempo))))))

(check-expect (line-posn 21000 160 0) (* (/ WIDTH (s (measure-length 160)))
                                         (modulo (round (- (+ 21000 0) SOUND-BUFFER (/ (s (measure-length 160)) SQRS)))
                                                 (round (s (measure-length 160))))))

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
