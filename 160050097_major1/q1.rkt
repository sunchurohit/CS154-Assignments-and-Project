#lang racket

(provide vertical-line)
         
(require plot)
(plot-new-window? #t)
(plot-width 900)
(plot-height 900)
(define (draw curve)
(plot (parametric
(lambda (t) (vector
(x-of (curve t))
(y-of (curve t))))
0 1 #:width 1 #:samples 20000
#:x-min -1 #:x-max 1
#:y-min -1 #:y-max 3)))

(define (make-point x y)
 (lambda (bit)
   (if (zero? bit) x y)))

(define (x-of point)
  (point 0))

(define (y-of point)
  (point 1))

(define (vertical-line p l)
  (lambda (t) (make-point (x-of p) t)
   (cond ((= t 0) p)
         ((= t 1) (make-point (x-of p) (+ l (y-of p))))
         (else (make-point (x-of p) (+ (* l t) (y-of p)))))))







