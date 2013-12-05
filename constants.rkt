;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname constants) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
(require "exporty.rkt")
(require rsound)
(require 2htdp/universe)
(require 2htdp/image)
(provide (all-defined-out))

; Buttons:
(define BUTTON-BROWN (bitmap/file "./Brown Button.jpg"))
(define BUTTON-GREEN (bitmap/file "./Green Button.png"))
(define BUTTON-PINK (bitmap/file "./Pink Button.png"))
(define BUTTON-BLUE (bitmap/file "./Blue Button.png"))
(define BUTTON-YELLOW (bitmap/file "./Yellow Button.png"))
(define BUTTON-RED (bitmap/file "./Red Button.png"))
(define BUTTON-LIGHTBLUE (bitmap/file "./Light Blue Button.png"))
(define BUTTON-LIGHTGREEN (bitmap/file "./Light Green Button.png"))
(define BUTTON-OFF (bitmap/file "./Off Button.png"))


; World
(define WIDTH 400)
(define HEIGHT 400)
(define W-WIDTH 600)
(define W-HEIGHT 600)
(define SQRS 8)
(define MT-SCN (empty-scene W-WIDTH W-HEIGHT))
(define SQR-SIZE (/ WIDTH 10))
(define SQR-DIST (/ (image-height BUTTON-GREEN) 2))
(define X-PAD (/ WIDTH (* 2 SQRS)))
(define DEFAULT-TEMPO 128)   ;;in unit of bpm
(define MINIMUM-TEMPO 60)
(define MAXIMUM-TEMPO 240)
(define DEFAULT-OFFSET 0)
(define OFFSET-STEP (* 44100 1/28))
(define (measure-length tempo) (/ 480 tempo))
(define SOUND-BUFFER (/ (* 44100 (measure-length DEFAULT-TEMPO)) 28))
(define BACKGROUND (bitmap/file "./Dr Ratchet Background.png"))
(define INSTRUCTIONS (bitmap/file "./Dr Ratchet Instructions.png"))




;Pstream
(define NOSOUND (silence 1))
(define row1sound (rs-scale 4/17 kick))
(define row2sound (rs-scale 4/17 bassdrum))
(define row3sound (rs-scale 10/17 bassdrum-synth))
(define row4sound (rs-scale 1/17 snare))
(define row5sound (rs-scale 1/17 clap-1))
(define row6sound (rs-scale 1/17 crash-cymbal))
(define row7sound (rs-scale 1/17 c-hi-hat-1))
(define row8sound (rs-scale 1/17 o-hi-hat))
(define ps (make-pstream))

; Convert Frames to Seconds
; number -> number
; returns a time value in frames from a time value in seconds
(define (s sec) (* 44100 sec))

(check-expect (s 0) 0)
(check-expect (s 1) 44100)



;Y Offset for Squares
; number -> number
; set the y offset value for each square
(define (y-offset n)
  (- (* n (/ WIDTH SQRS)) (/ WIDTH (* 2 SQRS))))

(check-expect (y-offset 5) (- (* 5 (/ WIDTH SQRS)) (/ WIDTH (* 2 SQRS))))
; X Offset for Squares
; number -> number
; set the x offset value for each square
(define (x-offset n)
  (- (* n (/ HEIGHT SQRS)) X-PAD))

(check-expect (x-offset 2) (- (* 2 (/ HEIGHT SQRS)) X-PAD))


; Maps a y coordinate of a square to a row
; number -> number
; separate scene into horizontal sections
(define (y-pt->y-gd y)
  (cond
    [(< 0 y (- (y-offset 1) SQR-DIST)) 0]
    [(< (+ (y-offset 1) SQR-DIST) y (- (y-offset 2) SQR-DIST)) -1]
    [(< (+ (y-offset 2) SQR-DIST) y (- (y-offset 3) SQR-DIST)) -2]
    [(< (+ (y-offset 3) SQR-DIST) y (- (y-offset 4) SQR-DIST)) -3]
    [(< (+ (y-offset 4) SQR-DIST) y (- (y-offset 5) SQR-DIST)) -4]
    [(< (+ (y-offset 5) SQR-DIST) y (- (y-offset 6) SQR-DIST)) -5]
    [(< (+ (y-offset 6) SQR-DIST) y (- (y-offset 7) SQR-DIST)) -6]
    [(< (+ (y-offset 7) SQR-DIST) y (- (y-offset 8) SQR-DIST)) -7]
    [(< (+ (y-offset 8) SQR-DIST) y HEIGHT) -8]
    [else (ceiling (* (/ y HEIGHT) SQRS))]))

(check-expect (y-pt->y-gd 31) 1)
(check-expect (y-pt->y-gd 46) -1)

; Maps a x coordinate of a square to a column
; number -> number
; separate scene into vertical sections
(define (x-pt->x-gd x)
  (cond
    [(< 0 x (- (x-offset 1) SQR-DIST)) -1]
    [(< (+ (x-offset 1) SQR-DIST) x (- (x-offset 2) SQR-DIST)) -1]
    [(< (+ (x-offset 2) SQR-DIST) x (- (x-offset 3) SQR-DIST)) -1]
    [(< (+ (x-offset 3) SQR-DIST) x (- (x-offset 4) SQR-DIST)) -1]
    [(< (+ (x-offset 4) SQR-DIST) x (- (x-offset 5) SQR-DIST)) -1]
    [(< (+ (x-offset 5) SQR-DIST) x (- (x-offset 6) SQR-DIST)) -1]
    [(< (+ (x-offset 6) SQR-DIST) x (- (x-offset 7) SQR-DIST)) -1]
    [(< (+ (x-offset 7) SQR-DIST) x (- (x-offset 8) SQR-DIST)) -1]
    [(< (+ (x-offset 8) SQR-DIST) x WIDTH) -1]
    [else (ceiling (* (/ x WIDTH) SQRS))]))

(check-expect (x-pt->x-gd 323) 7)
(check-expect (x-pt->x-gd 4) -1)





; a sq-part is (make-sq-part number posn boolean)
; -sq-part-len
; -sq-part-posn
; -sq-part-state
(define-struct sq-part (len posn state))


(define (both a b)
  b)