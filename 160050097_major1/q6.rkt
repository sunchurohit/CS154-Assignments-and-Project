#lang racket

(provide gosper-general)

(require plot)
(plot-new-window? #t)
(plot-width 900)
(plot-height 900)
(define (draw curve)
(plot (parametric
(lambda (t) (vector (x-of (curve t))
(y-of (curve t))))
0 1 #:width 1 #:samples 20000
#:x-min -2 #:x-max 2
#:y-min -3 #:y-max 3)))

(define (make-point x y)
(lambda (bit)
(if (zero? bit) x y)))

(define (x-of point)
(point 0))

(define (y-of point)
(point 1))

(define (rotate-around-origin radians curve)
  (lambda (t)
    (let ((ct (curve t)))
      (make-point (- (* (cos radians) (x-of ct)) (* (sin radians) (y-of ct))) (+ (* (sin radians) (x-of ct)) (* (cos radians) (y-of ct)))))))

(define (translate x y curve)
  (lambda (t)
    (let ((ct (curve t)))
      (make-point (+ x (x-of ct)) (+ y (y-of ct))))))

(define (scale x y curve)
  (lambda (t)
    (let ((ct (curve t)))
      (make-point (* x (x-of ct)) (* y (y-of ct))))))

(define (put-in-standard-position curve)
  (let* (
         (a (translate (- (x-of (curve 0))) (- (y-of (curve 0))) curve))
         (rad (if (= (x-of (a 1)) 0) (/ pi 2) (atan (/ (y-of (a 1)) (x-of (a 1))))))
         (b (rotate-around-origin (- rad) a))
         (c (if (> (x-of (b 1)) 0) (scale (/ 1 (x-of (b 1))) 1 b) (scale (/ 1 (- (x-of (b 1)))) 1 b)))
         )
    (lambda (t) (c t))))

(define (connect-ends curve1 curve2)
  (define y (translate (- (x-of (curve1 1)) (x-of (curve2 0))) (- (y-of (curve1 1)) (y-of (curve2 0))) curve2))
   (lambda (t)
     (if (< t 0.5) (curve1 (* 2 t))
         (y (- (* 2 t) 1)))))

(define (connect-rigidly curve1 curve2)
(lambda (t)
(if (< t (/ 1 2))
(curve1 (* 2 t))
(curve2 (- (* 2 t) 1)))))

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

(define (identity x) x)

(define (repeated f n)
(if (= n 0)
identity
(compose f (repeated f (- n 1)))))

(define onebysqrttwo (/ 1 (sqrt 2)))

(define (gosper-step-helper curve angle)
       (define a (scale onebysqrttwo onebysqrttwo curve))
       (define x (rotate-around-origin angle a))
       (define y (rotate-around-origin (- angle) a))
       (define z (translate (- 1 (x-of (y 1))) (- (y-of (y 1))) y))
       (define w (connect-ends x z))
       (define joker (put-in-standard-position w))
         (lambda (s)
           (joker s)))

(define (gosper-general curve calc-angle l)
  (define angle (calc-angle l))
  (define a
    (if (= l 0) curve
        (gosper-general (gosper-step-helper curve angle) calc-angle (- l 1))))
          (lambda (s)
            (a s)))
  

              

