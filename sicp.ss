#lang scheme
;; require MIT 6.034 compatibility library
;; (require (lib "init.ss" "6.034"))

;; http://www.gamedev.net/community/forums/topic.asp?topic_id=437195
(define (find-matching-item p? alist)
  (cond ((null? alist)
         #f)
        ((p? (car alist))
         (car alist))
        (else
         (find-matching-item p? (cdr alist)))))

(define (delete-matching-items p? alist)
  (define (helper accum alist)
    (cond ((null? alist)
           (reverse accum))
          ((p? (car alist))
           (helper accum (cdr alist)))
          (else
           (helper (cons (car alist) accum) (cdr alist)))))
  (helper (list) alist))

(define atom?
   (λ (a)
     (and (not (null? a)) (not (pair? a)) (not (list? a)))
   )
)


;lists of atoms used for testing
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


;is l a list of atoms?
(define lat?
  (λ (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f)
    )
  )
)


#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y))
)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2))
)

(define (avg x y)
  (/ (+ x y) 2)
)

;; average a list of numbers if arbitrary length (troubleshoot)
(define (avglat lat col)
  (cond
    ((null? (cdr lat) 0 0))
    (else (/ (+ (car lat) (car (cdr lat))) (avglat (cdr lat) (add1))))
  )
)

;; test for normal or applicative order evaluation
;; normal order: fully expand then reduce
;; applicative order:  evaluate arguments first then apply operators
;; if interpreter is applicative, it will evaluate (testp 0 (p)) to 0
;; if interpreter is normal, it will enter infite loop when subbing (p) for y)
(define (p) (p))
(define (testp x y)
  (if (= x 0) 0 y))

;; abs val x
(define (absx x)
  (if (< x 0) (- x) x)
)

;; >=
(define (gte x y)
  (or (x > y) (x = y))
)

;; <=
(define (lte x y)
  (not (x > y))
)

;; take 3 numbers and sum the square of the two largest
(define (ssql a b c)
  (if (> a b)
        (if (> a c)
              (if (> b c)
                    (+ (sqr a) (sqr b))
                    (+ (sqr a) (sqr c))
              )
              (+ (sqr a) (+ sqr c))
        )
        (if (or (> c b) (> c a)) (+ (sqr b) (sqr c))
              (+ (sqr b) (sqr c))
        )       
  )
)

;; do the same using an accumulator
;;(define (ssqr numlat)
  
;; define Square Root and all local subprocesses
(define (sqrtx x)
  
  ;; is our approximation of the sqrt of x good enough to return as the answer?
  ;; if x is within 0.001 of guess^2, return guess
  ;; else, continue recursing
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001)
  )
  
  ;; Newton's method for approximating the sqrt of x
  (define (improve guess x)
    (avg guess (/ x guess))
  )
  
  ;; the main function - if good enough return guess; else iterate again.
  (define (sqrtx-iterate guess x)
    (if (good-enough? guess x) guess (sqrtx-iterate (improve guess x) x))
  )
  
  ;; main
  (sqrtx-iterate 1.0 x)
)

;; Ackerman's function
(define (A x y)
  (cond
    ((= y 0) 0)
    ((= x 0) (* y 2))
    ((= y 1) 2)
    (else (A (- x 1) (A x (- y 1))))
  )
)

(define (fA n) (A 0 n))
(define (gA n) (A 1 n))
(define (hA n) (A 2 n))

;; recursive Fibonacci
(define (fib-r n)
  (cond
    ((= 0 n) 0)
    ((= 1 n) 1)
    (else (+ (fib-r (- n 1)) (fib-r (- n 2))))
  )
)

;; iterative Fibonacci
(define (fib-i n)
 
  ;; iterate
  (define (fib-iterate a b count)
    (if (= count 0) b
        (fib-iterate (+ a b) a (- count 1))
    )
  )

  ;; main
  (fib-iterate 1 0 n)
)

;; 1.11 recursive
(define (fnr n)
  (cond
    ((< n 3) n)
    (else (+ 0 (* n (fnr (- n (- n 1))))))
  )
)

;; 1.11 iterative
(define (fni n)
  
  (define (fni-iter n m count)
    (cond
      ((< n 3) n)
      (else (+ (* count (fni-iter ))))))

  (fni-iter n 0)
)

