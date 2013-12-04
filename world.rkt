;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname world) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
(require "exporty.rkt")
(require "constants.rkt")

(provide (all-defined-out))


; a world is (make-world sq-part frames boolean frames number number boolean)
(define-struct world (boxes time menu next-play-time tempo offset sp-b))

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