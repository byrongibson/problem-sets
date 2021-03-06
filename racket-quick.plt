#lang scheme
(require slideshow)
(require slideshow/flash)
(require slideshow/code)
(require scheme/class scheme/gui/base)
(require (planet "random.ss" ("schematics" "random.plt" 1 0)))

(provide rainbow square)

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
   (series (λ (sz) (colorize (mk sz) "red")))
   (series (λ (sz) (colorize (mk sz) "green")))
   (series (λ (sz) (colorize (mk sz) "blue")))))

(define (rgb-maker mk)
  (λ (sz)
    (vc-append (colorize (mk sz) "red")
               (colorize (mk sz) "green")
               (colorize (mk sz) "blue"))))

(define (rainbow p)
  (map (λ (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple")))

(define-syntax pict+code
  (syntax-rules ()
    [(pict+code expr)
     (hc-append 10
                expr
                (code expr))]))

(define f ( new frame% [label "My Art"]
                [width 300]
                [height 300]
                [alignment '(center center)]))

(define (add-drawing p)
  (let ([drawer (make-pict-drawer p)])
    (new canvas% [parent f]
         [style '(border)]
         [paint-callback (lambda (self dc)
                           (drawer dc 0 0))])))
