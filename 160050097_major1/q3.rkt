#lang racket

(provide connect-ends)

(require plot)
(plot-new-window? #t)
(plot-width 900)
(plot-height 900)
(define (draw curve)
(plot (parametric
(lambda (t) (vector (x-of (curve t))
(y-of (curve t))))
0 1 #:width 1 #:samples 20000
#:x-min 0 #:x-max 10
#:y-min 0 #:y-max 10)))

(define (make-point x y)
(lambda (bit)
(if (zero? bit) x y)))

(define (x-of point)
(point 0))

(define (y-of point)
(point 1))

(define (translate x y curve)
  (lambda (t)
    (let ((ct (curve t)))
      (make-point (+ x (x-of ct)) (+ y (y-of ct))))))

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

(define (connect-ends curve1 curve2)
  (define y (translate (- (x-of (curve1 1)) (x-of (curve2 0))) (- (y-of (curve1 1)) (y-of (curve2 0))) curve2))
   (lambda (t)
     (if (< t (/ 1 2)) (curve1 (* 2 t))
         (y (- (* 2 t) 1)))))
  

