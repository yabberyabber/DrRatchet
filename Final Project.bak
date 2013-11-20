;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Final Project|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
(require rsound)
(require 2htdp/universe)
(require 2htdp/image)
;(require "get-file.rkt")

;;;;;
;
;CONSTANTS
;
;;;;;
;(define USERSOUND (get-file))
;World
(define WIDTH 400)
(define HEIGHT 400)
(define W-WIDTH 600)
(define W-HEIGHT 450)
(define SQRS 8)
(define MT-SCN (empty-scene W-WIDTH W-HEIGHT))
(define SQR-SIZE (/ WIDTH 10))
(define X-PAD (/ WIDTH (* 2 SQRS)))
(define MEASURE-LENGTH 3)

(define row1color "blue")
(define row2color "red")
(define row3color "black")
(define row4color "violet")
(define row5color "lightblue")
(define row6color "darkblue")
(define row7color "lime")
(define row8color "yellow")
;Pstream
(define NOSOUND (silence 1))

(define row1sound (rs-scale 1/8 kick))
(define row2sound (rs-scale 1/8 bassdrum))
(define row3sound (rs-scale 1/8 bassdrum-synth))
(define row4sound (rs-scale 1/8 snare))
(define row5sound (rs-scale 1/8 clap-1))
(define row6sound (rs-scale 1/8 crash-cymbal))
(define row7sound (rs-scale 1/8 c-hi-hat-1))
(define row8sound (rs-scale 1/8 o-hi-hat))

;;;;;
;
;HELPER FUNCTIONS
;
;;;;;
;Frames to Seconds
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
;X Offset for Squares
; number -> number
; set the x offset value for each square
(define (x-offset n)
  (- (* n (/ HEIGHT SQRS)) X-PAD))

(check-expect (x-offset 2) (- (* 2 (/ HEIGHT SQRS)) X-PAD))

;rowNumber-> rsound
;Maps the row that a button is in
;to the sound file it is to play

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

;rowNumber->sound
;Maps the row that a button is in
;to the color it should change to

(define (mapRowtoColor row)
  (cond [(= row 1) row1color]
        [(= row 2) row2color]
        [(= row 3) row3color]
        [(= row 4) row4color]
        [(= row 5) row5color]
        [(= row 6) row6color]
        [(= row 7) row7color]
        [(= row 8) row8color]
        [else "black"]
        )
  )

(check-expect (mapRowtoColor 1) row1color)
(check-expect (mapRowtoColor 0) "black")

;Maps a y coordinate of a square to a row
; number -> number
; separate scene into horizontal sections
(define (y-pt->y-gd y)
  (cond
    [(< 0 y (- (y-offset 1) (/ SQR-SIZE 2))) -1]
    [(< (+ (y-offset 1) (/ SQR-SIZE 2)) y (- (y-offset 2) (/ SQR-SIZE 2))) -1]
    [(< (+ (y-offset 2) (/ SQR-SIZE 2)) y (- (y-offset 3) (/ SQR-SIZE 2))) -1]
    [(< (+ (y-offset 3) (/ SQR-SIZE 2)) y (- (y-offset 4) (/ SQR-SIZE 2))) -1]
    [(< (+ (y-offset 4) (/ SQR-SIZE 2)) y (- (y-offset 5) (/ SQR-SIZE 2))) -1]
    [(< (+ (y-offset 5) (/ SQR-SIZE 2)) y (- (y-offset 6) (/ SQR-SIZE 2))) -1]
    [(< (+ (y-offset 6) (/ SQR-SIZE 2)) y (- (y-offset 7) (/ SQR-SIZE 2))) -1]
    [(< (+ (y-offset 7) (/ SQR-SIZE 2)) y (- (y-offset 8) (/ SQR-SIZE 2))) -1]
    [(< (+ (y-offset 8) (/ SQR-SIZE 2)) y HEIGHT) -1]
    [else (ceiling (* (/ y HEIGHT) SQRS))]))

(check-expect (y-pt->y-gd 31) 1)
(check-expect (y-pt->y-gd 46) -1)
;;Maps a x coordinate of a square to a column
; number -> number
; separate scene into vertical sections
(define (x-pt->x-gd x)
  (cond
    [(< 0 x (- (x-offset 1) (/ SQR-SIZE 2))) -1]
    [(< (+ (x-offset 1) (/ SQR-SIZE 2)) x (- (x-offset 2) (/ SQR-SIZE 2))) -1]
    [(< (+ (x-offset 2) (/ SQR-SIZE 2)) x (- (x-offset 3) (/ SQR-SIZE 2))) -1]
    [(< (+ (x-offset 3) (/ SQR-SIZE 2)) x (- (x-offset 4) (/ SQR-SIZE 2))) -1]
    [(< (+ (x-offset 4) (/ SQR-SIZE 2)) x (- (x-offset 5) (/ SQR-SIZE 2))) -1]
    [(< (+ (x-offset 5) (/ SQR-SIZE 2)) x (- (x-offset 6) (/ SQR-SIZE 2))) -1]
    [(< (+ (x-offset 6) (/ SQR-SIZE 2)) x (- (x-offset 7) (/ SQR-SIZE 2))) -1]
    [(< (+ (x-offset 7) (/ SQR-SIZE 2)) x (- (x-offset 8) (/ SQR-SIZE 2))) -1]
    [(< (+ (x-offset 8) (/ SQR-SIZE 2)) x WIDTH) -1]
    [else (ceiling (* (/ x WIDTH) SQRS))]))

(check-expect (x-pt->x-gd 323) 7)
(check-expect (x-pt->x-gd 4) -1)


; a sq-part is (make-sq-part number posn boolean)
; -sq-part-len
; -sq-part-posn
; -sq-part-state
(define-struct sq-part (len posn state))


; a list-of-dims is one of:
; - empty, or
; (cons number list-of-dims)
; place square tiles and labels on background
; list-of-dims number -> image
(define (sqr-placer lod)
  (cond
    [(empty? lod) MT-SCN]
    [else (place-image (square
                        (sq-part-len (first lod))
                        (if (sq-part-state (first lod)) "solid" "outline")
                        (mapRowtoColor (y-pt->y-gd 
                                        (posn-y
                                         (sq-part-posn (first lod))))))
                       (posn-x (sq-part-posn (first lod)))
                       (posn-y (sq-part-posn (first lod)))
                                              
          (place-image (text "Kick" 20 "blue") 425 25
          (place-image (text "Bass Drum" 19 "red") 455 75             
          (place-image (text "Bass Drum Synth" 18 "black") 475 125
          (place-image (text "Snare" 19 "violet") 430 175
          (place-image (text "Clap" 20 "lightblue") 425 225
          (place-image (text "Clash Cymbal" 18 "darkblue")  460 275  
          (place-image (text "Closed Hi Hat" 18 "lime") 460 325
          (place-image (text "Open Hi Hat" 18 "yellow") 455 375
                        
                       (sqr-placer (rest lod)))))))))))  
          
          ])) 

(check-expect (sqr-placer (cons (make-sq-part 5
                                              (make-posn 10 20)
                                              true)
                                empty))
              (place-image (square 5 "solid" "blue")
                           10 20
                              (place-image (text "Kick" 20 "blue") 425 25
          (place-image (text "Bass Drum" 19 "red") 455 75             
          (place-image (text "Bass Drum Synth" 18 "black") 475 125
          (place-image (text "Snare" 19 "violet") 430 175
          (place-image (text "Clap" 20 "lightblue") 425 225
          (place-image (text "Clash Cymbal" 18 "darkblue")  460 275  
          (place-image (text "Closed Hi Hat" 18 "lime") 460 325
          (place-image (text "Open Hi Hat" 18 "yellow") 455 375))))))))))


; create-row takes an x and y and returns a list of 
; squares all with y-posn y and with x-posns ranging from 1 to x
; number number -> list-of-squares
(define (create-row x y)
  (cond [(equal? x 0) empty]
        [else (cons (make-sq-part SQR-SIZE (make-posn (x-offset x) (y-offset y)) false) (create-row (- x 1) y))]))

(check-expect (create-row 2 1)
              (cons (make-sq-part SQR-SIZE (make-posn (x-offset 2) (y-offset 1)) false)
                    (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false) empty)))
(check-expect (create-row 3 2)
              (cons (make-sq-part SQR-SIZE (make-posn (x-offset 3) (y-offset 2)) false)
                    (cons (make-sq-part SQR-SIZE (make-posn (x-offset 2) (y-offset 2)) false)
                          (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 2)) false) empty))))

; grid takes an x and y and returns a list of squares
; with y-posns ranging from 1 to y and x-posns ranging from 1 to x
; number number -> list-of-squares
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
                (cons (make-sq-part SQR-SIZE (sq-part-posn (first LOS)) (not (sq-part-state (first LOS)))) (toggle-square x y (rest LOS)))]
               [else (cons (first LOS) (toggle-square x y (rest LOS)))])]))


; test constants
(define LOB-EX (cons (make-sq-part SQR-SIZE (make-posn (x-offset 2) (y-offset 1)) false)
                     (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false)
                           (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 3)) false)
                                 (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 2)) false)
                                       (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false) empty))))))

(check-expect (toggle-square (- (x-offset 2) 1) (+ (y-offset 1) 1) LOB-EX)
              (cons (make-sq-part SQR-SIZE (make-posn (x-offset 2) (y-offset 1)) false)
                    (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false)
                          (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 3)) false)
                                (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 2)) false)
                                      (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false) empty))))))

;;;;;
;
;Mouse Handler
;
;;;;
; mouse handler handles mouse events.
; Depending on where the user clicked, toggles a square.
(define (me-h LOS x y event)
  (cond [(or (equal? event "button-down") (equal? event "drag"))
         (cond
           [(or (negative? (y-pt->y-gd y))
                (negative? (x-pt->x-gd x))) LOS]
           [else (toggle-square (x-pt->x-gd x) (y-pt->y-gd y) LOS)])]
        [else LOS]))

(check-expect (me-h LOB-EX (x-offset 1) (y-offset 2)  "button-down")
              (cons (make-sq-part SQR-SIZE (make-posn (x-offset 2) (y-offset 1)) false)
                    (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false)
                          (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 3)) false)
                                (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 2)) true)
                                      (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false) empty))))))
(check-expect (me-h LOB-EX (- (x-offset 2) 3) (+ (y-offset 1) 2) "button-down")
              (cons (make-sq-part SQR-SIZE (make-posn (x-offset 2) (y-offset 1)) true)
                    (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false)
                          (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 3)) false)
                                (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 2)) false)
                                      (cons (make-sq-part SQR-SIZE (make-posn (x-offset 1) (y-offset 1)) false) empty))))))
;;;;;
;
; ON TICK
;
;;;;;
(define (y-grid y)
  (* y (/ HEIGHT SQRS)))

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

; define pstream
(define ps (make-pstream))

; list-of-squares -> pstream
; queue a pstream depending on the time
(define (tick-handler los)
  (both (cond
          [(empty? los) ps]
          [(sq-part-state (first los)) (both (pstream-queue ps
                                                            (mapRowtoSound (y-pt->y-gd (posn-y (sq-part-posn (first los)))))
                                                            (round (+ (* (/ (s MEASURE-LENGTH) SQRS)
                                                                         (x-pt->x-gd (posn-x (sq-part-posn (first los)))))
                                                                      (+ (pstream-current-frame ps) 2400))))
                                             (tick-handler (rest los)))]
          [else (tick-handler (rest los))])
        los))

;;;;;
;
;BIG BANG
;
;;;;;
(big-bang (create-grid SQRS SQRS empty)
          [to-draw sqr-placer]
          [on-mouse me-h]
          [on-tick tick-handler MEASURE-LENGTH])