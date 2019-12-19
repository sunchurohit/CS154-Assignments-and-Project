#lang racket

(provide reflect-through-y-axis)
(provide translate)
(provide scale)
(provide rotate-around-origin)
(provide put-in-standard-position)

(require plot)
(plot-new-window? #t)
(plot-width 900)
(plot-height 900)
(define (draw curve)
(plot (parametric
(lambda (t) (vector (x-of (curve t))
(y-of (curve t))))
0 1 #:width 1 #:samples 20000
#:x-min -3 #:x-max 3
#:y-min -3 #:y-max 5)))

(define (make-point x y)
(lambda (bit)
(if (zero? bit) x y)))

(define (x-of point)
(point 0))

(define (y-of point)
(point 1))

(define (reflect-through-y-axis curve)
  (lambda (t)
    (let ((ct (curve t)))
    (make-point (- (x-of ct)) (y-of ct)))))

(define (translate x y curve)
  (lambda (t)
    (let ((ct (curve t)))
      (make-point (+ x (x-of ct)) (+ y (y-of ct))))))

(define (scale x y curve)
  (lambda (t)
    (let ((ct (curve t)))
      (make-point (* x (x-of ct)) (* y (y-of ct))))))

(define (rotate-around-origin radians curve)
  (lambda (t)
    (let ((ct (curve t)))
      (make-point (- (* (cos radians) (x-of ct)) (* (sin radians) (y-of ct))) (+ (* (sin radians) (x-of ct)) (* (cos radians) (y-of ct)))))))

(define (put-in-standard-position curve)
  (let* (
         (a (translate (- (x-of (curve 0))) (- (y-of (curve 0))) curve))
         (rad (if (= (x-of (a 1)) 0) (/ pi 2) (atan (/ (y-of (a 1)) (x-of (a 1))))))
         (b (rotate-around-origin (- rad) a))
         (c (if (> (x-of (b 1)) 0) (scale (/ 1 (x-of (b 1))) 1 b) (scale (/ 1 (- (x-of (b 1)))) 1 b)))
         )
    (lambda (t) (c t))))

(define (vertical-line p l)
  (lambda (t) (make-point (x-of p) t)
   (cond ((= t 0) p)
         ((= t 1) (make-point (x-of p) (+ l (y-of p))))
         (else (make-point (x-of p) (+ (* l t) (y-of p)))))))

(define (unit-line-at y)
    (lambda (t) (make-point t y)))
(define (unit-line) (unit-line-at 0))

(define (unit-circle)
(lambda (t)
(make-point (sin (* 2 pi t))
(cos (* 2 pi t)))))

(define (unit-semi-circle)
(lambda (t)
(make-point (sin (* pi t))
(cos (* pi t)))))

(define (connect-rigidly curve1 curve2)
(lambda (t)
(if (< t (/ 1 2))
(curve1 (* 2 t))
(curve2 (- (* 2 t) 1)))))
    


      
      
  




