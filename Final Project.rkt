;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Final Project|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
(require rsound)
(require 2htdp/universe)
(require 2htdp/image)
(require "get-file.rkt")

;;;;;
;
; CONSTANTS
;
;;;;;

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

; get-file
(define USER-SELECTION (get-file))
(define USERWAV (if (boolean? USER-SELECTION) (silence 1) (rs-scale 0.2 (rs-read USER-SELECTION))))


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

;;;;;
;
; HELPER FUNCTIONS
;
;;;;;

; Convert Frames to Seconds
; number -> number
; returns a time value in frames from a time value in seconds
(define (s sec) (* 44100 sec))

(check-expect (s 0) 0)
(check-expect (s 1) 44100)

(define (both a b)
  b)

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

; number -> rsound
; Maps the row that a button is in
; to the sound file it is to play

(define (mapRowtoSound row)
  (cond [(= row 1) row1sound]
        [(= row 2) row2sound]
        [(= row 3) row3sound]
        [(= row 4) row4sound]
        [(= row 5) row5sound]
        [(= row 6) row6sound]
        [(= row 7) row7sound]
        [(= row 8) row8sound]
        [else NOSOUND]
        )
  )
(check-expect (mapRowtoSound 1) row1sound)
(check-expect (mapRowtoSound 0) NOSOUND)

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

; a world is (make-world sq-part frames boolean frames number number boolean)
(define-struct world (boxes time menu next-play-time tempo offset sp-b))


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
                        (line-posn (- (+ DEFAULT-OFFSET (pstream-current-frame ps)) SOUND-BUFFER) 160 0)
                        0
                        (line-posn (- (+ DEFAULT-OFFSET (pstream-current-frame ps)) SOUND-BUFFER) 160 0)
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

;;function world-increment-tempo takes a world and returns the world with everything the same except with increased tempo
;; world -> world
(define (world-increment-tempo w)
  (make-world (world-boxes w)
              (world-time w)
              (world-menu w)
              (world-next-play-time w)
              (min MAXIMUM-TEMPO
                   (add1 (world-tempo w)))
              (world-offset w)
              (world-sp-b w)))

;;function world-decrement-tempo takes a world and returns the world with everything the same except with decreased tempo
;; world -> world
(define (world-decrement-tempo w)
  (make-world (world-boxes w)
              (world-time w)
              (world-menu w)
              (world-next-play-time w)
              (max MINIMUM-TEMPO 
                   (sub1 (world-tempo w)))
              (world-offset w)
              (world-sp-b w)))

;;function world-increment-offset takes a world and returns it with incremented offset
;; world -> world
(define (world-increment-offset w)
  (make-world (world-boxes w)
              (world-time w)
              (world-menu w)
              (world-next-play-time w)
              (world-tempo w)
              (+ (world-offset w) OFFSET-STEP)
              (world-sp-b w)))

;;function world-decrement-offset takes a world and returns it with decremented offset
;; world -> world
(define (world-decrement-offset w)
  (make-world (world-boxes w)
              (world-time w)
              (world-menu w)
              (world-next-play-time w)
              (world-tempo w)
              (- (world-offset w) OFFSET-STEP)
              (world-sp-b w)))


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

; a list-of-squares is one of:
; - empty, or
; - (cons sq-part list-of-squares)
; list-of-squares -> rsound
; to map each column with an rsound for the tick handler
(define (tick-helper los)
    (cond
    [(empty?) (silence 1)]
    [(= (posn-x (sq-part-posn (first los))) (x-grid 1)) row1sound]
    [(= (posn-x (sq-part-posn (first los))) (x-grid 2)) row2sound]
    [(= (posn-x (sq-part-posn (first los))) (x-grid 3)) row3sound]
    [(= (posn-x (sq-part-posn (first los))) (x-grid 4)) row4sound]
    [(= (posn-x (sq-part-posn (first los))) (x-grid 5)) row5sound]
    [(= (posn-x (sq-part-posn (first los))) (x-grid 6)) row6sound]
    [(= (posn-x (sq-part-posn (first los))) (x-grid 7)) row7sound]
    [(= (posn-x (sq-part-posn (first los))) (x-grid 8)) row8sound]
    [else (silence 1)]))


; list-of-squares number tempo -> pstream
; given a list of boxes, a play offset, and a tempo, queues the upcomming active boxes
(define (queuer los offset tempo)
  (local
    [(define current-time (+ (modulo (pstream-current-frame ps) (round (s (measure-length tempo)))) SOUND-BUFFER))]
    (cond
      [(empty? los) ps]
      [(and (play-yet? current-time offset (x-pt->x-gd (posn-x (sq-part-posn (first los)))) tempo)
            (sq-part-state (first los))) 
       (pstream-queue (queuer (rest los) offset tempo)
                      (mapRowtoSound (y-pt->y-gd (posn-y (sq-part-posn (first los)))))
                      (round 
                       (next-time-to-play 
                        (pstream-current-frame ps) 
                        offset
                        (x-pt->x-gd (posn-x (sq-part-posn (first los))))
                        tempo)))]
      [else (queuer (rest los) offset tempo)])))
   
;; next-time-to-play given a box and the current time will determine the next time that box will play
;; number number -> number
(define (next-time-to-play now offset col tempo)
  (+ (* (floor (/ now (s (measure-length tempo)))) (s (measure-length tempo)))
     (* col (s (measure-length tempo)) 1/8)
     SOUND-BUFFER
     offset))

; world -> world
; change the time in the world
(define (world-time-change w)
  (make-world (world-boxes w)
              (add1 (world-time w))
              (world-menu w)
              (+ (world-next-play-time w) (s (/ 1 28)))
              (world-tempo w)
              (world-offset w)
              (world-sp-b w)))

; world frame -> boolean
; tell if it is time to play or not
(define (play-yet? curr-time offset col tempo)
  (and (> curr-time (* (/ (s (measure-length tempo)) SQRS) col))
       (< curr-time (next-time-to-play (pstream-current-frame ps) offset col tempo))))

(check-expect (play-yet? 22050 20000 0 160) true)
(check-expect (play-yet? 22050 2100 250 180) false)

; world -> world
; queue a pstream depending on the time and update time of world
(define (tick-handler w)
  (if (world-menu w)
      w
      (both (queuer (world-boxes w) (world-offset w) (world-tempo w))
            (world-time-change w))))

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
