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

#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y))
)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2))
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