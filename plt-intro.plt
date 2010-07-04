#lang scheme

(require slideshow)
(require slideshow/flash)

(define (c r)
  (circle r))

(define (r l w)
  (rectangle l w))

(define (square n)
  (filled-rectangle n n))

(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))

(define (checker p1 p2)
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))

(define (checkerboard p)
  (let* ([rp (colorize p "red")]
          [bp (colorize p "black")]
          [c (checker rp bp)]
          [c4 (four c)])
    (four c4)))

(define (series mk)
  (hc-append 4 (mk 5) (mk 10) (mk 20)))
  
(define seriesl
  (λ (mk)
    (hc-append 4 (mk 5) (mk 10) (mk 20))))

(define (rgb-series mk)
  (vc-append 
   (series (lambda (sz) (colorize (mk sz) "red")))
   (series (lambda (sz) (colorize (mk sz) "green")))
   (series (lambda (sz) (colorize (mk sz) "blue")))))

(define (rgb-maker mk)
  (λ (sz)
    (vc-append (colorize (mk sz) "red")
               (colorize (mk sz) "green")
               (colorize (mk sz) "blue"))))

(define (rainbow p)
  (map (λ (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple")))

