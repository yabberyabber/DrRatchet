;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname sounding) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
(require "exporty.rkt")
(require rsound)
(require "constants.rkt")
(require "world.rkt")

(provide (all-defined-out))

(define (both a b)
  b)

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
