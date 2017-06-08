#lang racket
(require "testcases.rkt")
(require "declaration.rkt")
(require racket/gui)

(provide iter state draw-animation new-list)

(define iter 300)
(define state 'pause)
(define rate .02)
(define scale 1)
(define new-list '())
;define alive dead and create in declarations.rkt

(define frame (new frame% [label "Conway's Game of Life"]
                   [width 1100]
                   [height 700]
                   [x 0]
                   [y 0]))

(define main-panel (new horizontal-panel%
                        [parent frame]))

(define simulation_panel (new panel%
                              [alignment '(left top)]
                              [min-width 800]
                              [min-height 600]
                              [parent main-panel]
                              ))

(define simulation-canvas (new canvas% [parent simulation_panel]
                               [paint-callback
                                (lambda (canvas dc) (paint dc))]))

(define setting_panel (new vertical-panel%
                           [alignment '(center center)]
                           [min-width 300]
                           [min-height 600]
                           [parent main-panel]
                           ))

(define rule-frame (new frame% [label "Rules"]
                        [width 500][height 100][x 90][y 20]))

(define dead-panel (new horizontal-panel% [parent rule-frame]
                        [alignment '(center top)]
                        [min-width 500]
                        [min-height 50]))

(define alive-panel (new horizontal-panel% [parent rule-frame]
                         [alignment '(center bottom)]
                         [min-width 500]
                         [min-height 50]))

(define alive-text (new message%
                        [label "Number of live neighbours for alive cells :        "]
                        [parent alive-panel]
                        ))

(define dead-text (new message%
                        [label "Number of live neighbours for dead cells :        "]
                        [parent dead-panel]
                        ))

(define dead1 (new check-box% [parent dead-panel] [label "1"]))
(define dead2 (new check-box% [parent dead-panel] [label "2"]))
(define dead3 (new check-box% [parent dead-panel] [label "3"]))
(define dead4 (new check-box% [parent dead-panel] [label "4"]))
(define dead5 (new check-box% [parent dead-panel] [label "5"]))
(define dead6 (new check-box% [parent dead-panel] [label "6"]))
(define dead7 (new check-box% [parent dead-panel] [label "7"]))
(define dead8 (new check-box% [parent dead-panel] [label "8"]))
(define alive1 (new check-box% [parent alive-panel] [label "1"]))
(define alive2 (new check-box% [parent alive-panel] [label "2"]))
(define alive3 (new check-box% [parent alive-panel] [label "3"]))
(define alive4 (new check-box% [parent alive-panel] [label "4"]))
(define alive5 (new check-box% [parent alive-panel] [label "5"]))
(define alive6 (new check-box% [parent alive-panel] [label "6"]))
(define alive7 (new check-box% [parent alive-panel] [label "7"]))
(define alive8 (new check-box% [parent alive-panel] [label "8"]))

(define (create-rule)
  (begin 
    (let* [(i 9)
           (temp-alive-list 
            (foldr (lambda(val y)(begin (set! i (- i 1)) (if val (cons i y) y) )) `()
                   (list (send alive1 get-value)
                         (send alive2 get-value)
                         (send alive3 get-value)
                         (send alive4 get-value)
                         (send alive5 get-value)
                         (send alive6 get-value)
                         (send alive7 get-value)
                         (send alive8 get-value))))]
      (mutate-alive temp-alive-list))
    (let* [(i 9)
           (temp-dead-list 
            (foldr (lambda(val y)(begin (set! i (- i 1)) (if val (cons i y) y) )) `()
                   (list (send dead1 get-value)
                         (send dead2 get-value)
                         (send dead3 get-value)
                         (send dead4 get-value)
                         (send dead5 get-value)
                         (send dead6 get-value)
                         (send dead7 get-value)
                         (send dead8 get-value))))]
      (mutate-dead temp-dead-list))))

(define done (new button%
                  [label "Done"]
                  [parent rule-frame]
                  [callback (lambda(button event) (begin (create-rule) (send rule-frame show #f)))]
                  ))

(define choice-frame (new frame% [label "List"]
                          [width 500][height 100][x 90][y 20]))

(define choice-list (new radio-box%
                         [label ""]
                         [parent choice-frame]
                         [choices '("Cross" "Fly" "Puffer" "Replicator" "Slowpuffer")]
                         [callback (lambda (b e)
                                     (let ([event (send b get-selection)])
                                       (begin (send choice-frame show #f)
                                              (set! state 'change)
                                              (cond
                                                [(= event 0) (set! new-list cross)]
                                                [(= event 1) (set! new-list fly)]
                                                [(= event 2) (set! new-list puffer)]
                                                [(= event 3) (set! new-list replicator)]
                                                [(= event 4) (set! new-list slowpuffer)]
                                                ))))]))
                                              
                         


(define setting_canvas%
  (class canvas%
    (define/override (on-event event)
      (let ((x (if (eq? 'left-down (send event get-event-type)) (send event get-x) 0))
            (y (if (eq? 'left-down (send event get-event-type)) (send event get-y) 0)))
        (cond
          [(and (> x 50) (< x 97) (> y 255) (< y 302)) (set! state 'resume)]
          [(and (> x 130) (< x 177) (> y 255) (< y 302)) (set! state 'pause)]
          [(and (> x 205) (< x 252) (> y 255) (< y 302)) (set! state 'reset)]
          [(and (> x 50) (< x 97) (> y 338) (< y 385)) (set! rate (* rate 1.5))]
          [(and (> x 130) (< x 177) (> y 338) (< y 385)) (set! rate (/ rate 1.5))]
          [(and (> x 50) (< x 97) (> y 419) (< y 472)) (set! scale (* scale 1.5))]
          [(and (> x 130) (< x 177) (> y 419) (< y 472)) (set! scale (/ scale 1.5))]
          [(and (> x 80) (< x 225) (> y 533) (< y 567)) (begin (set! state 'reset) (send rule-frame show #t))]
          [(and (> x 80) (< x 225) (> y 600) (< y 648)) (begin (set! state 'pause) (send choice-frame show #t) )]
          )))(super-new)))

(define setting-canvas (new setting_canvas% [parent setting_panel]
                            [paint-callback (λ (c d)
                                              (send d draw-bitmap (read-bitmap "settings.bmp") 0 0))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (paint dc) (send dc draw-bitmap face-bitmap 0 0))
(define face-bitmap (make-object bitmap% 800 800))
(define bm-dc (make-object bitmap-dc% face-bitmap))
(send bm-dc clear)

(define white-brush (make-object brush% "WHITE" 'solid)) ; WHITE means "live" cell
(define black-pen (make-object pen% "BLACK" 0 'solid))

(send bm-dc set-background "BLACK")

(send frame show #t)

(define (draw-animation listofcells) 
  (begin
    (cond[(or (equal? state 'reset) (equal? state 'change))
          (begin (set! scale 1) (set! state 'pause) (set! new-list '())
          (set! rate 0.02) )])
    (send bm-dc clear)
    (send bm-dc set-brush white-brush)
    ;(send bm-dc set-pen black-pen)
    (map (lambda (p) (let* ((x (car p))
                            (y (cadr p)))
                       (send bm-dc draw-rectangle (+ 70 (* x 4 scale)) (+ 200 (* y 4 scale)) (* scale 5) (* scale 5)))) listofcells)
    ;(send bm-dc draw-rectangle (+ (* x 5) (* (cos (/ pi 3)) (* 5 y))) (* (sin (/ pi 3)) (* y 5)) 6 6))) listofcells)
    (send simulation-canvas refresh)
    (sleep/yield rate))
  )