#lang racket

(require racket/gui)

(define fr (new frame%
               [label "Planetary orbital simulator"]
               [width  900]
              [height 500]
               ))

(define mph (new horizontal-panel% [parent fr]
                 [min-height 500]
                 [min-width 900]))

(define mphl (new vertical-panel% [parent mph] [min-height 500] [min-width 600]))
(define mphr (new vertical-panel% [parent mph] [min-height 500] [min-width 300]))

(send fr show #t)

(define canvas (new canvas% [parent mphl] [min-height 500]
                    [paint-callback
        (lambda (canvas dc) (paint dc))]))
(define (paint dc) (send dc draw-bitmap face-bitmap 0 0))

(send canvas set-canvas-background (make-object color% 0 0 0 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define animation-controls (new vertical-panel% [parent mphr] ))
                                                              
(define orbit-settings (new vertical-panel% [parent mphr] ))
                                                 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define animation-controls-heading (new panel%
                                        [parent animation-controls]
                                        [min-height 60]))

(define animation-controls-tail (new vertical-panel%
                                     [parent animation-controls]
                                     [min-height 190]))

(define act-values (new vertical-panel% [parent animation-controls-tail] [min-height 80]))

(define orbit-settings-heading (new panel%
                                    [parent orbit-settings]
                                    [min-height 60] ))
                                    
(define orbit-settings-tail (new vertical-panel%
                                 [parent orbit-settings]
                                 [min-height 190]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ft (make-object font%))

(define ac-heading (new message%
                        [font ft]
                        [parent animation-controls-heading]
                        [label "Animation Controls"]))

(define os-heading (new message%
                        [font ft]
                        [parent orbit-settings-heading]                        
                        [label "Orbit Settings"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ar 1.00)

(define animation-rate (new message% [parent act-values]
                            [label (string-append "animation rate(yrs/s) : " (number->string ar) "    ")] ))

(define ar-slider (new slider%
                       [parent act-values]
                       [label #f]
                       [min-value 2]
                       [max-value 200]
                       [min-height 70]
                       [callback (lambda (b e)
                                   (begin
                                      (set! ar (send ar-slider get-value))                                      
                                      (send animation-rate set-label (string-append "animation rate(yrs/s) : "
                                                                                    (number->string (inexact->exact (/ ar 10))) "    "))))]
                       [horiz-margin 50]))

(define start-animation (new button%
                             [parent animation-controls-tail]
                             [label "Start Animation"]
                             [callback (lambda (b x) (main (ps sma e ar v)))]))

;(define pause/resume (new button% [parent animation-controls-tail] [label "pause/resume"]
;                          [callback (lambda (b x) ()))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define au 149597870700)
(define sma 149597870700)
(define v 29300)

(define planet (new choice%
                    [parent orbit-settings-tail]
                    [label "planet"]
                    [choices (list "Mercury" "Venus" "Earth" "Mars")]
                    [callback (lambda (c x) 
                                (cond
                                [(equal? (send c get-string-selection) "Mercury") (set! v 38860)
                                                                                  (set! sma (* 0.387 au))
                                                                                  (set! e 0.206)
                                                                                  (send sma-slider set-value (inexact->exact (floor (* (/ sma au) 10))))
                                                                                  (send e-slider set-value (inexact->exact (* e 1000)))
                                                                                  (send semimajor-axis set-label (string-append "semimajor-axis(AU) : " (number->string (exact->inexact (/ sma au))) "    "))
                                                                                  (send eccentricity set-label (string-append "eccentricity : " (number->string e) "      "))]                                         
                                [(equal? (send c get-string-selection) "Venus") (set! v 34800)
                                                                                (set! sma (* 0.723 au))
                                                                                (set! e 0.007)
                                                                                (send sma-slider set-value (inexact->exact (floor (* 10 (/ sma au)))))
                                                                                (send e-slider set-value (inexact->exact (* e 1000)))
                                                                                (send semimajor-axis set-label (string-append "semimajor-axis(AU) : " (number->string (exact->inexact (/ sma au))) "    "))
                                                                                (send eccentricity set-label (string-append "eccentricity : " (number->string e) "      "))]
                                [(equal? (send c get-string-selection) "Earth") (set! v 29300)
                                                                                (set! sma au)
                                                                                (set! e 0.017)
                                                                                (send sma-slider set-value (inexact->exact (floor (* 10 (/ sma au)))))
                                                                                (send e-slider set-value (inexact->exact (* e 1000)))
                                                                                (send semimajor-axis set-label (string-append "semimajor-axis(AU) : " (number->string (exact->inexact (/ sma au))) "    "))
                                                                                (send eccentricity set-label (string-append "eccentricity : " (number->string e) "      "))]
                                [(equal? (send c get-string-selection) "Mars") (set! v 22000)
                                                                               (set! sma (* au 1.52))
                                                                               (set! e 0.093)
                                                                               (send sma-slider set-value (inexact->exact (floor (* 10 (/ sma au)))))
                                                                               (send e-slider set-value (inexact->exact (* e 1000)))
                                                                               (send semimajor-axis set-label (string-append "semimajor-axis(AU) : " (number->string (exact->inexact (/ sma au))) "    "))
                                                                               (send eccentricity set-label (string-append "eccentricity : " (number->string e) "      "))])
                                )]
                    [min-height 50]))

(send planet set-selection 2)

(define semimajor-axis (new message%
                            [parent orbit-settings-tail]
                            [label (string-append "semimajor-axis(AU) : "
                                                  (number->string (exact->inexact (/ sma au))) "      ")] ))

(define sma-slider (new slider%
                        [parent orbit-settings-tail]
                        [label #f]
                        [min-value 1]
                        [max-value 50]
                        [callback (lambda (b e) (begin
                                                  (set! sma (* (send sma-slider get-value) au))
                                                  (send semimajor-axis set-label (string-append "semimajor-axis(AU) : "
                                                                                                (number->string (exact->inexact (/ sma au 10))) "      "))))]
                        [horiz-margin 50]))

(define e 000.000)

(define eccentricity (new message%
                          [parent orbit-settings-tail]
                          [label (string-append "eccentricity : " (number->string e) "       ")] ))

(define e-slider (new slider%
                      [parent orbit-settings-tail]
                      [label #f]
                      [min-value 0]
                      [max-value 500]
                      [callback (lambda (b x) (begin
                                                  (set! e (exact->inexact (/ (send e-slider get-value) 1000)))
                                                  (send eccentricity set-label (string-append "eccentricity : " (number->string e) "      "))))]
                      [horiz-margin 50]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;

(struct vec (x y) #:transparent)
(struct particle (time-period posn vel a e r t) #:transparent)

;;;;;;;;GLOBAL DECLARATIONS

(define timeslice 0.01)
(define iter 50000)
(define drawtime 1)

;;;;;;;;;

(define (square x) (* x x))

(define (atan1 y x a e);redefining atan
  (let* ((y1 (- y 250))
         (x1 (- (- x 250) (* a e))))
        (cond ((and (= y1 0) (= x1 0)) 0)
              ((>= y1 0) (atan y1 x1))
              ((< y1 0) (+ (* 2 pi) (atan y1 x1))))))
        

(define (r a b theta);;r = distance b/w planet and origin.
  (sqrt (+ (expt (* a (cos theta)) 2) (expt (* b (sin theta)) 2))))

(define (R a e theta);;R = distance b/w planet and sun.
  (* a (+ 1 (* (cos theta) e))))

(define (vcap a b theta);;vcap = direction of velocity vector.
  (let ((mag (r a b theta)))
   (vec (/ (- (* b (sin theta))) mag) (/ (* a (cos theta)) mag))))
 
(define (sphi a e theta);;sphi = sin of phi.
  (/ (* (sqrt (- 1 (* e e))) (sin theta)) (+ 1 (* e (cos theta)))))

(define (Rcap a e theta);;Rcap = direction of Rbar.
  (let* ((sp (sphi a e theta)))
    (vec (sqrt (- 1 (expt sp 2))) sp)))

(define (area a b);area of the ellipse
  (* pi a b))

(define (cprod a b);cprod = vector cross product of 2 vectors abar and bbar.
  (abs (- (* (vec-x a) (vec-y b)) (* (vec-y a) (vec-x b)))))

(define (cpsi a e theta);;cpsi = cos of psi.
  (let* ((b (* a (sqrt (- 1 (expt e 2))))) 
         (vc (vcap a b theta))
             (Rc (Rcap a e theta)))
             (cprod vc Rc)))

(define (velocity a e theta avel);;vel = velocity magnitude of planet. 
    (let* ((R1 (R a e theta))
           (cp (cpsi a e theta)))
             (/ (* 2 avel) (* R1 cp))))

(define (velocity-vector a e theta avel);;velocity vector.
    (let* ((mag (velocity a e theta avel))
          (b (* a (sqrt (- 1 (square e)))))
          (direction (vcap a b theta)))
      (vec (* mag (vec-x direction)) (* mag (vec-y direction)))))

(define (mag v)
  (sqrt (+ (* (vec-x v) (vec-x v)) (* (vec-y v) (vec-y v)))))

(define (avel p);aerial velocity of a planet
   (let* ((theta (atan1 (vec-y (particle-posn p)) (vec-x (particle-posn p))
                                                    (particle-a p) (particle-e p)))
          (a (particle-a p))
          (e (particle-e p)))
     (* 0.5 (R a e theta) (cprod (Rcap a e theta) (particle-vel p)))))
          
(define framesize 500)
(define bitmap-size 600)

; ... pens, brushes, and draw-face are the same as above ...

; Create a bitmap
(define face-bitmap (make-object bitmap% bitmap-size bitmap-size))

; Create a drawing context for the bitmap
(define bm-dc (make-object bitmap-dc% face-bitmap))
(send bm-dc set-background (make-object color% 0 0 0))
; A bitmap's initial content is undefined; clear it before drawing
(send bm-dc clear)

; Make some pens and brushes
(define white-pen (make-object pen% "WHITE" 1 'solid))
(define black-brush (make-object brush% "BLACK" 'solid))
(define red-pen (make-object pen% "WHITE" 2 'solid))

;;Change this to get object sizes to your liking

(define scale-radius 20)
; Show the frame
;(send frame show #t)
  ;  draw-particles :: [(Radius, Posn)] -> Action

(define (draw-particles l)
  (begin
    (send bm-dc clear) 
    (send bm-dc set-brush black-brush)
    (send bm-dc set-pen red-pen)
    (map (lambda (p) (let*(
                           [a (particle-a p)]
                           [e (particle-e p)]
                           [b (* a (sqrt (- 1 (* e e))))]
                           [posn (particle-posn p)]
                           [diameter scale-radius ]
                           [x (- (vec-x posn) (/ diameter 2))]
                           [y (- (vec-y posn) (/ diameter 2))]
                            )
                       (if (= 0 (particle-t p))
                           (send bm-dc draw-ellipse (- (+ 250 (* a e)) a) (- 250 b) (* 2 a) (* 2 b))
                           (send bm-dc draw-ellipse x y diameter diameter)))) l)
     (send canvas refresh)
    (sleep/yield 0.01)))

(define (moveparticle p)
  (define new-posn (vec (+ (vec-x (particle-posn p))
                              (* timeslice (vec-x (velocity-vector (particle-a p) (particle-e p)
                                 (atan1 (vec-y (particle-posn p)) (vec-x (particle-posn p))
                                        (particle-a p) (particle-e p)) (avel p)))))
                           (+ (vec-y (particle-posn p)) (* timeslice (vec-y (velocity-vector (particle-a p)
                              (particle-e p) (atan1 (vec-y (particle-posn p)) (vec-x (particle-posn p))
                                                    (particle-a p) (particle-e p)) (avel p)))))))
  (define approx-posn (vec (+ 250 (* (particle-a p) (particle-e p))
                              (* (particle-a p) (cos (atan1 (vec-y new-posn) (vec-x new-posn) (particle-a p) (particle-e p)))))
                           (+ 250 (* (particle-a p) (sin (atan1 (vec-y new-posn) (vec-x new-posn) (particle-a p) (particle-e p)))
                              (sqrt (- 1 (square (particle-e p))))))))  
  (define new-vel (velocity-vector (particle-a p) (particle-e p)
                                   (atan1 (vec-y approx-posn) (vec-x approx-posn)
                                          (particle-a p) (particle-e p)) (avel p)))
  (particle (particle-time-period p) approx-posn new-vel (particle-a p) (particle-e p) (particle-r p) (particle-t p)))

(define (moveparticles l)
  (map moveparticle l))

(define (scale p)
  (let* ((tp (particle-time-period p))
         (new-posn (vec (+ 450 (* 200 (particle-e p))) 250))
         (new-vel (vec (* (/ (* 200 (particle-r p) 31536000) (particle-a p)) (vec-x (particle-vel p)))
                   (* (/ (* 200 (particle-r p) 31536000) (particle-a p)) (vec-y (particle-vel p))))))
    (particle tp new-posn new-vel 200 (particle-e p) (particle-r p) (particle-t p))))
                   
(define (main x)
  (define (main-helper p i)
    (cond [(> i iter) (display "Done")]
          [else (let*
                    ([p-next (moveparticles p)])
                  (if (= (remainder iter drawtime) 0)
                      (begin 
                        (draw-particles p)
                        (main-helper p-next (+ i 1)))
                      (main-helper p-next (+ i 1))))]))
  (main-helper x 0))

(define (move p i)
  (if (= i 0)
      '()
      (cons (moveparticle p) (move (moveparticle p) (- i 1)))))

(define (ps a e ar v) (list                       
                       (particle 1 (vec 10 10) (vec 0 0) 200 e 0 0)
                       (scale (particle 1 (vec 10 10) (vec 0 v)  #|149597870700|# a e ar 1))
                       (particle 0.24 (vec 10 10) (vec 0 0) 20 0 1 0)
                        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define mercury
;  (list (particle 0.24 (vec 10 10) (vec 0 0) 200 0.206 1 0)
;         (scale (particle 0.24 (vec 10 10) (vec 0 38860) (* 0.387 au) 0.206 1 1))
;         (particle 0.24 (vec 10 10) (vec 0 0) 20 0 1 0)))
;(define venus
;  (list (particle (* 16 0.0384) (vec 10 10) (vec 0 0) 200 0.007 1 0)
;         (scale (particle (* 16 0.0384) (vec 10 10) (vec 0 34800) (* 0.723 au) 0.007 1 1))
;         (particle 0.24 (vec 10 10) (vec 0 0) 20 0 1 0)))
;(define earth
;  (list (particle 1 (vec 10 10) (vec 0 0) 200 0.017 1 0)
;         (scale (particle 1 (vec 10 10) (vec 0 29300) au 0.017 1 1))
;         (particle 0.24 (vec 10 10) (vec 0 0) 20 0 1 0)))
;(define mars
;  (list (particle (* 16 0.118) (vec 10 10) (vec 0 0) 200 0.093 1 0)
;         (scale (particle (* 16 0.118) (vec 10 10) (vec 0 22000) (* au 1.52) 0.093 1 1))
;         (particle 0.24 (vec 10 10) (vec 0 0) 20 0 1 0)))