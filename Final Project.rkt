(require rsound)
(require 2htdp/universe)
(require "get-file.rkt")
(require 2htdp/image)
;run this with advanced student

(define usersound (get-file))
(define WIDTH 400)
(define HEIGHT 400)
(define SQRS 8)
(define MT-SCN (empty-scene WIDTH HEIGHT))
(define SQR-SIZE (/ WIDTH 10))
(define X-PAD (/ WIDTH (* 2 SQRS)))

(define row1sound kick)
(define row2sound bassdrum)
(define row3sound bassdrum-synth)
(define row4sound snare)
(define row5sound clap-1)
(define row6sound crash-cymbal)
(define row7sound c-hi-hat-1)
(define row8sound o-hi-hat)

(define row1color "blue")
(define row2color "red")
(define row3color "black")
(define row4color "violet")
(define row5color "lightblue")
(define row6color "darkblue")
(define row7color "lime")
(define row8color "yellow")


; number -> number
; set the y offset value for each square
(define (y-offset n)
  (- (* n (/ WIDTH SQRS)) (/ WIDTH (* 2 SQRS))))

(check-expect (y-offset 5) (- (* 5 (/ WIDTH SQRS)) (/ WIDTH (* 2 SQRS))))

; number -> number
; set the x offset value for each square
(define (x-offset n)
  (- (* n (/ HEIGHT SQRS)) X-PAD))

(check-expect (x-offset 2) (- (* 2 (/ HEIGHT SQRS)) X-PAD))


; dummy functions

;Maps the row that a button is in
;to the sound file it is to play
;rowNumber->sound
(define (mapRowtoSound row)
  (cond [(= row 1) row1sound]
        [(= row 2) row2sound]
        [(= row 3) row3sound]
        [(= row 4) row4sound]
        [(= row 5) row5sound]
        [(= row 6) row6sound]
        [(= row 7) row7sound]
        [(= row 8) row8sound]
        [else usersound]
        )
  )
(check-expect (mapRowtoSound 1) row1sound)
(check-expect (mapRowtoSound 0) usersound)

;Maps the row that a button is in
;to the color it should change to
;rowNumber->sound
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
                        (if (sq-part-state (first lod)) "solid" "outline")
                        (mapRowtoColor (y-pt->y-gd 
                                        (posn-y
                                         (sq-part-posn (first lod))))))
                       (posn-x (sq-part-posn (first lod)))
                       (posn-y (sq-part-posn (first lod)))
                       (sqr-placer (rest lod)))]))

(check-expect (sqr-placer (cons (make-sq-part 5
                                              (make-posn 10 20)
                                              true)
                                empty))
              (place-image (square 5 "solid" "blue")
                           10 20
                           MT-SCN))


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

(define (both a b)
  b)


; mouse handler handles mouse events.
; Depending on where the user clicked, toggles a square.
(define (me-h LOS x y event)
  (cond [(equal? event "button-down")
         (both (play (mapRowtoSound (y-pt->y-gd y)))
               (cond
                 [(or (negative? (y-pt->y-gd y))
                      (negative? (x-pt->x-gd x))) LOS]
                 [else (toggle-square (x-pt->x-gd x) (y-pt->y-gd y) LOS)]))]
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


;big bang
(big-bang (create-grid SQRS SQRS empty)
          [to-draw sqr-placer]
          [on-mouse me-h])
