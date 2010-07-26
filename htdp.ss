;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname htdp) (read-case-sensitive #t) (teachpacks ((lib "convert.ss" "teachpack" "htdp") (lib "guess.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp") (lib "hangman.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "convert.ss" "teachpack" "htdp") (lib "guess.ss" "teachpack" "htdp") (lib "draw.ss" "teachpack" "htdp") (lib "hangman.ss" "teachpack" "htdp")))))
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

(define (area-of-ring outer inner)
  (- (area-of-disk outer)
     (area-of-disk inner)
  )
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

;; Section 3: Program Composition

;; compute the movie theater profit as the difference between costs and revenues at a given ticket price
;; profit = revenue - costs
;; revenue = (number of tickets sold) * (ticket price)
;; cost = (180) + (.04 * number of tickets sold)
;; decreasing price by .10 increases tickets sold by 15

(define (movie-attendance p)
  (+ 120 (* (/ 15 .10) (- 5.00 p)))
)

(define (movie-revenue p)
  (* p (movie-attendance p))
)

(define (movie-cost p)
  (+ 180 (* .04 (movie-attendance p)))
)

(define (movie-profit p)
  (- (movie-revenue p) (movie-cost p))
)

;; Section 4: Conditional Expressions & Functions

;is x a solution to x^2 + 2 * x + 1 = 0?
(define (eqt1 x)
  (= (+ (* x x) (* x 2) 1) 0)
)

;; (4.3.3)
(define (interest-rate d)
  (cond
    [(<= d 1000) 0.040]
    [(<= d 5000) 0.045]
    [(> d 5000) 0.050]
  )
)

;; (6.1)
;; distance-to-0 takes a-posn (cartesian coordinate x,y) and calculates its distance to origin 0,0
;; make-posn and a-posn are defined in the teachpack
;; d = (sqrt ((sqr x) + (sqr y)))
(define (distance-to-0 p)
  (cond
    [(number? p) p]
    [(posn? p) (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p))))]
  )
)

;; (6.3)
(define-struct entry (name zip phone))


;; struct star : firstname lastname instrument sales
(define-struct star (last first instrument sales))

;; increment-sales : star -> star
;; to produce a star record like a-star with 20000 more sales
(define (increment-sales a-star)
  (make-star (star-last a-star)
             (star-first a-star)
             (star-instrument a-star)
             (+ (star-sales a-star) 20000)
  )
)

;; draw a traffic light
;; dimensions of traffic light
(define WIDTH 50)
(define HEIGHT 160)
(define BULB-RADIUS 20)
(define BULB-DISTANCE 10)

;; positions of bulbs
(define X-BULBS (quotient WIDTH 2))
(define Y-RED (+ BULB-DISTANCE BULB-RADIUS))
(define Y-YELLOW (+ Y-RED BULB-DISTANCE (* 2 BULB-RADIUS)))
(define Y-GREEN (+ Y-YELLOW BULB-DISTANCE (* 2 BULB-RADIUS)))

;; draw the light with the red bulb turned on
;; (define (draw-traffic-light)
  ;;(start WIDTH HEIGHT)
  ;;(draw-solid-disk (make-posn X-BULBS Y-RED) BULB-RADIUS 'red)
  ;;(draw-circle (make-posn X-BULBS Y-YELLOW) BULB-RADIUS 'yellow)
  ;;(draw-circle (make-posn X-BULBS Y-GREEN) BULB-RADIUS 'green)
;; )


;; two shapes square, circle
;; nw = northwest corner (origin of a square)
;; o = origin of a circle
(define-struct square (nw len))
(define-struct circle (o r))

;; perimeter : shape -> number
;; compute the perimter of a-shape
(define (perimeter a-shape)
  (cond
    [(square? a-shape) (* 4 (square-len a-shape))]
    [(circle? a-shape) (* pi (* 2 (circle-r a-shape)))]
  )
)

;; f : shape -> ???
;; 
(define (f a-shape)
  (cond
    [(square? a-shape) ... ]
    [(circle? a-shape) ... ]
  )
)

