#lang racket

(require "declaration.rkt")
(require "ReadWrite.rkt")
;(require "testcases.rkt")
(provide evolve)

;(define alive-list (list 2 3))
;(define dead-list (list 3))
        
(define (NextGen-Base nw ne sw se Hash)
  (define (evolve Leaf neighbours)
    (if (= Leaf 1)
        (if (ormap (λ(x) (eq? x neighbours)) alive-list)            (leaf 1)            (leaf 0))
        (if (ormap (λ(x) (eq? x neighbours)) dead-list)            (leaf 1)            (leaf 0))))
  
  (let*
      [;(n1 (node-nw nw))       (n2 (node-ne nw))       (n3 (node-nw ne))       (n4 (node-ne ne))
       ;(n5 (node-sw nw))       (n6 (node-se nw))       (n7 (node-sw ne))       (n8 (node-se ne))
       ;(n9 (node-nw sw))       (n10 (node-ne sw))      (n11 (node-nw se))       (n12 (node-ne se))
       ;(n13 (node-sw sw))       (n14 (node-se sw))       (n15 (node-sw se))       (n16 (node-se se))
       (neighbour-n6 (foldr + 0 (list
                                 (leaf-value (node-nw nw))                   (leaf-value (node-ne nw))
                                 (leaf-value (node-nw ne))                   (leaf-value (node-sw nw))
                                 (leaf-value (node-sw ne))                   (leaf-value (node-nw sw))
                                 (leaf-value (node-ne sw))                   (leaf-value (node-nw se)))))
       (neighbour-n7 (foldr + 0 (list
                                 (leaf-value (node-ne nw))                   (leaf-value (node-nw ne))
                                 (leaf-value (node-ne ne))                   (leaf-value (node-se nw))
                                 (leaf-value (node-se ne))                   (leaf-value (node-ne sw))
                                 (leaf-value (node-nw se))                   (leaf-value (node-ne se)))))
       (neighbour-n10 (foldr + 0 (list
                                  (leaf-value (node-sw nw))                  (leaf-value (node-se nw));n6)
                                  (leaf-value (node-sw ne))                  (leaf-value (node-nw sw));n9)
                                  (leaf-value (node-nw se))                  (leaf-value (node-sw sw));n13)
                                  (leaf-value (node-se sw))                  (leaf-value (node-sw se)))));n15))))
       (neighbour-n11 (foldr + 0 (list
                                  (leaf-value (node-se nw))                  (leaf-value (node-sw ne));n7)
                                  (leaf-value (node-se ne))                  (leaf-value (node-ne sw));n10)
                                  (leaf-value (node-ne se))                  (leaf-value (node-se sw));n14)
                                  (leaf-value (node-sw se))                  (leaf-value (node-se se)))));n16))))
       (NW (evolve (leaf-value (node-se nw)) neighbour-n6))
       (NE (evolve (leaf-value (node-sw ne)) neighbour-n7))
       (SW (evolve (leaf-value (node-ne sw)) neighbour-n10))
       (SE (evolve (leaf-value (node-nw se)) neighbour-n11))
       (future    (findNode NW NE SW SE Hash))]
    
    (hash-set! Hash (list (node-key nw) (node-key ne) (node-key sw) (node-key se)) (node nw ne sw se (gensym) future))
    future))
    
(define (NextGen nw ne sw se Hash)
  (let [(quadtree (findNode nw ne sw se Hash))]
    (if (null? (node-next quadtree))
        (if (<= (size quadtree) 4)
            (NextGen-Base nw ne sw se Hash)
            (let* [
                   (n1 (findNode (node-se (node-nw nw)) (node-sw (node-ne nw)) (node-ne (node-sw nw)) (node-nw (node-se nw)) Hash))
                   (n2 (findNode (node-se (node-ne nw)) (node-sw (node-nw ne)) (node-ne (node-se nw)) (node-nw (node-sw ne)) Hash))
                   (n3 (findNode (node-se (node-nw ne)) (node-sw (node-ne ne)) (node-ne (node-sw ne)) (node-nw (node-se ne)) Hash))
                   (n4 (findNode (node-se (node-sw nw)) (node-sw (node-se nw)) (node-ne (node-nw sw)) (node-nw (node-ne sw)) Hash))
                   (n5 (findNode (node-se (node-se nw)) (node-sw (node-sw ne)) (node-ne (node-ne sw)) (node-nw (node-nw se)) Hash))
                   (n6 (findNode (node-se (node-sw ne)) (node-sw (node-se ne)) (node-ne (node-nw se)) (node-nw (node-ne se)) Hash))
                   (n7 (findNode (node-se (node-nw sw)) (node-sw (node-ne sw)) (node-ne (node-sw sw)) (node-nw (node-se sw)) Hash))
                   (n8 (findNode (node-se (node-ne sw)) (node-sw (node-nw se)) (node-ne (node-se sw)) (node-nw (node-sw se)) Hash))
                   (n9 (findNode (node-se (node-nw se)) (node-sw (node-ne se)) (node-ne (node-sw se)) (node-nw (node-se se)) Hash))
                   (NW (NextGen n1 n2 n4 n5 Hash))
                   (NE (NextGen n2 n3 n5 n6 Hash))
                   (SW (NextGen n4 n5 n7 n8 Hash))
                   (SE (NextGen n5 n6 n8 n9 Hash))
                   (future (findNode NW NE SW SE Hash))
                   ]
              (hash-set! Hash (list (node-key nw) (node-key ne) (node-key sw) (node-key se)) (node nw ne sw se (node-key quadtree) future))
              future
              ))
        (node-next quadtree))))

(define (trim future Hash)
  (if (is-nulltree future) (create-nulltree 8 Hash)
      (if (< (size future) 16) future
          (let*
              [(L1 (node-nw future))
               (L2 (node-ne future))
               (L3 (node-sw future))
               (L4 (node-se future))]
            (if (andmap is-nulltree (list
                                     (node-nw L1) (node-ne L1) (node-sw L1)
                                     (node-nw L2) (node-ne L2) (node-se L2)
                                     (node-nw L3) (node-sw L3) (node-se L3)
                                     (node-ne L4) (node-sw L4) (node-se L4)))
                (findNode (node-se L1) (node-sw L2) (node-ne L3) (node-nw L4) Hash)
                future)))))
               

(define (expand universe Hash)
  (let*
      [(sze (size universe))
       (nulltree (create-nulltree (/ sze 2) Hash))
       (NW (findNode nulltree nulltree nulltree (node-nw universe) Hash))
       (NE (findNode nulltree nulltree (node-ne universe) nulltree Hash))
       (SW (findNode nulltree (node-sw universe) nulltree nulltree Hash))
       (SE (findNode (node-se universe) nulltree nulltree nulltree Hash))]
    (findNode NW NE SW SE Hash)))

(define (evolve quadtree Hash)
  (let* [
         (tree (expand quadtree Hash))
         (tree (expand tree Hash))
         (future (NextGen (node-nw tree) (node-ne tree) (node-sw tree) (node-se tree) Hash))
         ]
;    (display alive-list)
;    (display dead-list)
    future))
;(trim future)))
;(define (loop N i)
;  (define ht (make-hash))
;  (define (loo N i)
;    (if (= i 0) N
;        (loop (evolve N ht) (- i 1))))
;  (loo N i))
;(define SpaceHash (make-hash))
;(populate SpaceHash)
;(define x (create-nulltree 2 SpaceHash))
;(define y (create-nulltree 4 SpaceHash))
;(define NW-small (findNode (leaf 0) (leaf 0) (leaf 1) (leaf 1) SpaceHash)) 
;(define NW (findNode x x x NW-small SpaceHash))
;(define NE-small (findNode (leaf 0) (leaf 0) (leaf 1) (leaf 0) SpaceHash))
;(define NE (findNode x x NE-small x SpaceHash))
;(define N (findNode NW NE y y SpaceHash))