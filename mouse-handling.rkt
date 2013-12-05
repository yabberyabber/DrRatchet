;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname mouse-handling) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
(require "exporty.rkt")
(require "constants.rkt")
(require "world.rkt")
(require "grid-tools.rkt")

(provide me-h)


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

;; check that clickin ga box toggle's it's state
(check-expect (me-h (make-world (create-grid 3 3 empty) 0 false 0 DEFAULT-TEMPO DEFAULT-OFFSET false) (x-offset 1) (y-offset 2)  "button-down")
              (make-world 
               (toggle-square 1 2 (create-grid 3 3 empty))
               0 false 0 DEFAULT-TEMPO DEFAULT-OFFSET false))

;; check that clicking between spots doesn't change any boxes
(check-expect (me-h (make-world (create-grid 3 3 empty) 0 false 0 DEFAULT-TEMPO DEFAULT-OFFSET false) (- (x-offset 2) 3) (+ (y-offset 1) 2) "button-down")
              (make-world 
               (toggle-square 2 1 (create-grid 3 3 empty))
               0 false 0 DEFAULT-TEMPO DEFAULT-OFFSET false))