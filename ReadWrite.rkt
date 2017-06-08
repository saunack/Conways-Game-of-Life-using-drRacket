#lang racket

(require "declaration.rkt")
;(require "testcases.rkt")

(provide tree-to-list)
(provide list-to-tree)

(define (to-list tree x y size List)
  (cond
    [(leaf? tree) (if (= (leaf-value tree) 1) (cons (list (inexact->exact (- x 0.5)) (inexact->exact (+ y 0.5))) List) List)]
    [(is-nulltree tree) List]
    [else
     (let* [
            (buff-list1 (to-list (node-nw tree) (- x (/ size 4)) (- y (/ size 4)) (/ size 2) List))
            (buff-list2 (to-list (node-ne tree) (+ x (/ size 4)) (- y (/ size 4)) (/ size 2) buff-list1))
            (buff-list3 (to-list (node-sw tree) (- x (/ size 4)) (+ y (/ size 4)) (/ size 2) buff-list2))
            (buff-list4 (to-list (node-se tree) (+ x (/ size 4)) (+ y (/ size 4)) (/ size 2) buff-list3))
            ]
       buff-list4
       )]))

(define (tree-to-list quadtree)
  (to-list quadtree 0 0 (size quadtree) `()))

(define (bgrid List Hash)
  (let* [(N (foldr (λ(X Y) (max (abs (car X)) (abs (cadr X)) Y)) 0 List))
        (size (if (= N 0) 1(inexact->exact (+ (ceiling (/ (log N) (log 2))) 1))))]
    (create-nulltree (expt 2 size) Hash)))

(define (update Hash tree)
  (cond
    [(leaf? tree) tree]
    [(is-nulltree tree) tree]
    [else (let* [
                 (NW (update Hash (node-nw tree)))
                 (NE (update Hash (node-ne tree)))
                 (SW (update Hash (node-sw tree)))
                 (SE (update Hash (node-se tree)))]
            (findNode NW NE SW SE Hash))
            ]
            ))

(define (to-tree List bounding-grid)
  (define (add tree x y cell size)
    (if (leaf? tree) (leaf 1)
        (let* [(X (- (car cell) x))
               (Y (- (cadr cell) y))]
          (cond
            [(and (>= X 0) (> Y 0)) (node (node-nw tree) (node-ne tree) (node-sw tree) (add (node-se tree) (+ (/ size 4) x) (+ y (/ size 4)) cell (/ size 2)) null null)]
            [(and (< X 0) (> Y 0)) (node (node-nw tree) (node-ne tree) (add (node-sw tree) (- x (/ size 4)) (+ y (/ size 4)) cell (/ size 2)) (node-se tree) null null)]
            [(and (< X 0) (<= Y 0)) (node (add (node-nw tree) (- x (/ size 4)) (- y (/ size 4)) cell (/ size 2)) (node-ne tree) (node-sw tree) (node-se tree) null null)]
            [(and (>= X 0) (<= Y 0)) (node (node-nw tree) (add (node-ne tree) (+ (/ size 4) x) (- y (/ size 4)) cell (/ size 2)) (node-sw tree) (node-se tree) null null)])
          )))
  (foldr (λ(X Y) (add Y 0 0 X (size bounding-grid))) bounding-grid List))

(define (list-to-tree List Hash)
  (begin
    (mutate-alive (cadr List))
    (mutate-dead (car List))
    (let* [
           (coordinates (cddr List))
           (blankgrid (bgrid coordinates Hash))
           (initial-tree (to-tree coordinates blankgrid))
           ]
    (update Hash initial-tree))))

;(define L `((2 3) (3 6) (0 3) (1 2) (2 1) (2 0) (-1 0) (0 -1) (1 -1) (2 -1) (-2 1) (-2 2) (-2 3) (-1 3)))
;(define L `((2 3) (3 6) (0 0) (0 1) (-1 0) (-1 1)))
;(define x (nulltree 2))
;(define y (nulltree 4))
;(define NW (node 4 (list x (node 2 (list (leaf 1) (leaf 1) (leaf 0) (leaf 0))) x x)))
;(define NE (node 4 (list (node 2 (list (leaf 1) (leaf 0) (leaf 0) (leaf 0))) x x x)))
;(define N (node 8 (list NW NE y y)))
;(define SpaceHash (make-hash))
;(populate SpaceHash)
;(define x (create-nulltree 2 SpaceHash))
;(define y (create-nulltree 4 SpaceHash))
;(define NW-small (findNode (leaf 0) (leaf 0) (leaf 1) (leaf 1) SpaceHash)) 
;(define NW (findNode x x x NW-small SpaceHash))
;(define NE-small (findNode (leaf 0) (leaf 0) (leaf 1) (leaf 0) SpaceHash))
;(define NE (findNode x x NE-small x SpaceHash))
;(define N (findNode NW NE y y SpaceHash))