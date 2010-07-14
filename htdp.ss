;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname htdp) (read-case-sensitive #t) (teachpacks ((lib "convert.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.ss" "teachpack" "htdp")))))
;insert λ in DrScheme/DrRacket with Ctrl+\.

;comment key words:
;(troubleshoot) : not working, review and fix
;(difficult)    : didn't get it, had to look at answer, review
;(p.1234)       : page in How to Design Programs this code appears on

;required primitives not implemented in DrScheme/DrRacket
(define atom?
   (λ (a)
     (and (not (null? a)) (not (pair? a)))
   )
)

(define (area-of-disk r)
    (* 3.14 (* r r))
)

;Fahrenheit->Celcius
(define (F-C f)
    (* (- f 32) (/ 5 9))
)

;Celcius->Fahrenheit
(define (C-F c)
    (+ (* c (/ 9 5)) 32)
)


(define (somef x)
  (sin x x)
)

(define (wage h)
    (* 12 h))