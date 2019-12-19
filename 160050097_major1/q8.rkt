#lang racket

(provide sierpinski-curve)

(require plot)
(plot-new-window? #t)
(plot-width 900)
(plot-height 900)
(define (draw curve)
(plot (parametric
(lambda (t) (vector (x-of (curve t))
(y-of (curve t))))
0 1 #:width 1 #:samples 20000
#:x-min -0.5 #:x-max 1.5
#:y-min -0.5 #:y-max 1.5)))

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

(define unit-triangle
  (lambda (t)
    (cond ((and (>= t 0) (< t 0.5)) (a (* 2 t))) 
          (else (make-point (- (* 2 t) 1) 0)))))

(define first-line
  (lambda (t)
    (make-point (/ t 2) (/ (* t (sqrt 3)) 2))))

(define second-line
  (lambda (t)
    (make-point (/ (+ t 1) 2) (/ (* (- 1 t) (sqrt 3)) 2))))

(define a (connect-ends first-line second-line))

(define (reflect-through-x-axis curve)
  (lambda (t)
    (let ((ct (curve t)))
    (make-point (x-of ct) (- (y-of ct))))))

(define (reflect-through-y-axis curve)
  (lambda (t)
    (let ((ct (curve t)))
    (make-point (- (x-of ct)) (y-of ct)))))

(define (sierpinski-step curve)
  (let* [
         (a (scale 0.5 0.5 curve))
         (b (translate 0.25 (/ (sqrt 3) 4) a))
         (c (translate 0.5 0 a))
         (d (lambda (t)
              (cond ((and (>= t 0) (< t (/ 1 3))) (a (* 3 t)))
                    ((and (>= t (/ 1 3)) (< t (/ 2 3))) (b (* 3 (- t (/ 1 3)))))
                    (else (c (* 3 (- t (/ 2 3))))))))
         ]
    (lambda (t) (d t))))

(define (sierpinski-curve level)
  ((repeated sierpinski-step level) unit-triangle))

    