#lang racket
(provide (all-defined-out))
(define livecell `((1) (2) (0 0)))
(define livecells1 (list (list 2 3) (list 2 3) (list 0 0) (list 1 1) (list 2 1) (list 2 0) (list 2 -1)))
(define livecells2 (list (list 3) (list 2 3) (list 50 50) (list 50 51) (list 51 50) (list 51 51) (list 60 50) (list 60 51) (list 60 52) (list 61 49) (list 61 53) (list 62 48)
                        (list 63 48) (list 62 54) (list 63 54) (list 64 51) (list 65 49) (list 65 53) (list 66 50) (list 66 51) (list 66 52) (list 67 51) (list 70 50)
                        (list 70 49) (list 70 48) (list 71 50) (list 71 49) (list 71 48) (list 72 51) (list 72 47) (list 74 46) (list 74 47) (list 74 51) (list 74 52)
                        (list 84 48) (list 85 48) (list 84 49) (list 85 49)))
(define livecells3 (append `((3)) `((2 3)) (map (lambda (l) (list (car l) (last l))) (append* (map (lambda (x) (append (list (list (car x) (- 100 (last x)))) (list x)))
                                                                         '((10 39) (10 43) (10 44) (11 40) (11 43) (11 44)
                                                                                   (12 40) (12 43) (13 40) (14 40) (14 45) (14 46)
                                                                                   (14 48) (15 37) (15 40) (15 46) (15 47) (15 48)
                                                                                   (16 38) (16 39) (16 40) (16 47)))))))
(define livecells4 (list (list 3) (list 2 3) (list 50 50) (list 50 48) (list 50 52) (list 49 48) (list 49 52) (list 51 48) (list 51 52)))






