#lang racket

(require "declaration.rkt")
(require "drawing-routine.rkt")
;(require "test.rkt")
(require "testcases.rkt")
(require "code.rkt")
(require "ReadWrite.rkt")

(define (main Init [exit? #f])
  (define ht (make-hash))
  (define (main-helper init i)
    (cond [(= i 0) (begin (draw-animation (tree-to-list init))
                          (cond
                            [(equal? 'resume state) (main-helper init (+ 1 i))]
                            [(equal? 'pause state) (main-helper init i)]
                            [(equal? 'reset state) (main Init #f)]
                            [(equal? 'change state) (main new-list #f)]
                            ))]
          [(<= i iter) (let* [(next (evolve init ht))]
                         (begin
                           (draw-animation (tree-to-list next))
                           (cond
                             [(equal? 'resume state) (main-helper next (+ 1 i))]
                             [(equal? 'pause state) (main-helper init i)]
                             [(equal? 'reset state) (main Init #f)]
                             [(equal? 'change state) (main new-list #f)]
                             )))]
          [(> i iter) (begin (draw-animation (tree-to-list init)) (main-helper init i)
                             (cond
                             [(equal? 'reset state) (main Init #f)]
                             [(equal? 'change state) (main new-list #f)]
                             ))]))
  (if exit? null
      (main-helper (list-to-tree Init ht) 0)))

;(define (run)
(time (main title))
;(time (main (list-to-tree (list (list 0 0) (list 1 1)) ht)))
;)