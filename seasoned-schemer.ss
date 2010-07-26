#lang scheme

;insert λ in DrScheme/DrRacket with Ctrl+\.

;comment key words:
;(troubleshoot) : not working, review and fix
;(difficult)    : didn't get it, had to look at answer, review
;(p.1234)       : page in The Seasoned Schemer this code appears on

;required primitives not implemented in DrScheme/DrRacket
(define atom?
   (λ (a)
     (and (not (null? a)) (not (pair? a)) (not (list? a)))
   )
)

;lists of lists of atoms used for testing
(define lat
  '(an oldish sort of oldish long list of oldish atoms))
(define latlat
  '(((an)) (oldish (sort of)) oldish (long (list of)) oldish atoms))
(define nulllatlat
  '(((()))((an)) (oldish (sort of)) ((())) oldish (long (list of(()))) oldish atoms))
(define numlat
  '(5 pears 6 prunes 9 dates))
(define commanumlat
  '(3 blind mice, 3 blind mice))
(define looklat1
  '(6 2 4 caviar 5 7 3))
(define looklat2
  '(7 1 2 caviar 5 6 3))

;tups used for testing
(define tup
  '(1 2 3 4 5))
(define negTup
  '(1 -2 3 -4 5))
(define t1
  '(3 7))
(define t2
  '(4 6))
(define t3
  '(3 7 8 1))
(define t4
  '(4 6 8 1))

;sets used for testing
(define pair
  '(1 2))
(define twopair
  '((1 2) (3 4)))
(define pairs
  '(one (two)))
(define spairs
  '((one) (two)))
(define pairset
  '((a b c d) (e f g h)))
(define nums
  '(0 1 2 3 4 5 6 7 8 9))
(define numsnums
  '(0 (1 2 3) 4 5 (6 7 (8 9))))
(define lset
  '((a b c)
    (c a d e)
    (e f g h a b))
)
(define lsets
  '((6 pears and)
    (3 peaches and 6 peppers)
    (8 pears and 6 plums)
    (and 6 prunes with some apples))
)

;rels used for testing
;fun but not a fullfun
(define rel1
  '((8 3) (4 2) (7 6) (6 2) (3 4)))
;fullfun
(define rel2
  '((9 1) (8 2) (7 3) (6 4) (5 0)))
;fullfun
(define rel3
  '((0 9) (1 8) (2 7) (3 6) (4 5)))