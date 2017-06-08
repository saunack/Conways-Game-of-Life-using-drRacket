#lang racket

(provide (struct-out node) (struct-out leaf) drawtime findNode create-nulltree is-nulltree size populate alive-list dead-list mutate-alive mutate-dead)

(struct vec (x y) #:transparent)

(struct node (nw ne sw se key next) #:transparent)
(struct leaf (value) #:transparent)

(define alive-list `(2 3))
(define dead-list `(3))

(define (mutate-alive NewList)
  (set! alive-list NewList))
(define (mutate-dead NewList)
  (set! dead-list NewList))

(define (populate Hash)
  (begin
    (hash-set! Hash 0 (leaf 0))
    (hash-set! Hash 1 (leaf 1))
    (create-nulltree 8 Hash)
    ;(foldr (λ(a A)
    ;        (foldr (λ(b A)
    ;                (foldr (λ(c A)
    ;                        (foldr (λ(d A)
    ;                                (findNode (leaf a) (leaf b) (leaf c) (leaf d) Hash)) `() (list 0 1))
    ;                      ) `() (list 0 1))) `() (list 0 1))) `() (list 0 1))
    (display "Hash table populated")
    (newline)
    ))

(define (findNode nw ne sw se Hash)
  (if (is-nulltree (node nw ne sw se `() `()))
          (create-nulltree (* 2 (size nw)) Hash)
          (if (leaf? nw)
              (let [(quadtree (hash-ref Hash (list (leaf-value nw) (leaf-value ne) (leaf-value sw) (leaf-value se)) null))]
                (if (null? quadtree)
                    (let [(quadtree (node nw ne sw se (gensym) null))]
                      (hash-set! Hash (list (leaf-value nw) (leaf-value ne) (leaf-value sw) (leaf-value se)) quadtree)
                      quadtree)
                    quadtree))
              (let [(quadtree (hash-ref Hash (list (node-key nw) (node-key ne) (node-key sw) (node-key se)) null))]
                (if (null? quadtree)
                    (let [(quadtree (node nw ne sw se (gensym) null))]
                      (hash-set! Hash (list (node-key nw) (node-key ne) (node-key sw) (node-key se)) quadtree)
                      quadtree)
                    quadtree)))))

(define (create-nulltree size Hash)
  (if (eq? size 2) (begin
                     (hash-set! Hash 2 (node (leaf 0) (leaf 0) (leaf 0) (leaf 0) 2 null))
                     (node (leaf 0) (leaf 0) (leaf 0) (leaf 0) 2 null)
                     )
      (if (null? (hash-ref Hash size null))
          (let* [(nulltree (create-nulltree (/ size 2) Hash))
                 (ans (node nulltree nulltree nulltree nulltree size nulltree))]
            (hash-set! Hash size ans)
            ans)
          (hash-ref Hash size))))
            
(define (is-nulltree tree)
  (if (leaf? tree) #f
      (if (leaf? (node-nw tree))
          (andmap (λ(x) (eq? 0 (leaf-value x))) (list (node-nw tree) (node-ne tree) (node-sw tree) (node-se tree)))
          (if (and
               (integer? (node-key (node-nw tree)))
               (integer? (node-key (node-ne tree)))
               (integer? (node-key (node-sw tree)))
               (integer? (node-key (node-se tree)))) #t #f))))

(define (size Tree)
  (define (traverse tree ans)
    (cond
      [(leaf? tree) ans]
      [(is-nulltree tree) (* ans (node-key tree))]
      [else (or
             (traverse (node-nw tree) (* 2 ans))
             (traverse (node-ne tree) (* 2 ans))
             (traverse (node-sw tree) (* 2 ans))
             (traverse (node-se tree) (* 2 ans)))]))
  (traverse Tree 1))

(define drawtime 1)


