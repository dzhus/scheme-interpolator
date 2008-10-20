#lang scheme/gui

(require srfi/1)

(require plot
         plot/extend)

(require "interpolate.ss"
         "point.ss")

(define (steps) 500)

(define (mark-radius) 2)

;; A canvas which shows a part of cartesian plane. The class knows how
;; to translate clicks on canvas to cartesian coordinates (represented
;; with `point` structure) and is also capable of converting these
;; coordinates to `point%` instances (which may be further drawn with
;; `draw-lines` from `scheme/plot`)
(define plane-frame%
  (class canvas%
    (inherit get-client-size get-dc)
    
    ;; Bounds of currently visible plane frame
    (init-field x-min y-min x-max y-max)
    
    (super-new)

    (define/public (point->point% point)
      (let-values (((width height) (get-client-size)))
        (let* ((x (point-x point))
               (y (point-y point))
               (client-x (* (/ (- x x-min)
                               (- x-max x-min))
                            width))
               (client-y (* (/ (- y-max y)
                               (- y-max y-min))
                            height)))
          (make-object point% client-x client-y))))
    
    (define/public (click->point click-event)
      (let-values (((width height) (get-client-size)))
        (let* ((click-x (send click-event get-x))
               (click-y (send click-event get-y))
               (x-factor (/ click-x width))
               (y-factor (/ click-y height))
               (x (+ x-min (* 1.0 x-factor (- x-max x-min))))
               (y (- y-max (* 1.0 y-factor (- y-max y-min)))))
          (make-point x y))))))

;; A choice widget which maps selected items to interpolation methods
(define method-choice%
  (class choice%
    (super-new)
    
    (define/public (get-methods)
      (list
       (let ((n (send this get-selection)))
         ;; @TODO make this list an initialization variable
         (list-ref (list lagrange-lambda-interpolation
                         polynomial-interpolation
                         spline-interpolation)
                   n))))))

(define interpolation-pad%
  (class plane-frame%
    (inherit click->point point->point% get-dc)
    (inherit-field x-min x-max)

    (init-field method-chooser)
    (super-new)

    (define points '())

    (define/public (clear-points)
      (set! points '()))

    (define (interpolation-result->points% res steps)
      (define (scalar f range)
        (map (lambda (p) (point->point% p))
             (function->grid f range)))
      (define (vector f range)
        (map (lambda (v) (point->point% (vector->point v)))
             (map f range)))
      (if (eq? 'vector (interpolation-result-type res))
          (vector (interpolation-result-function res)
                       (append
                        (map (lambda (t) (/ t steps)) (iota steps))
                        (list 1)))
          (scalar (interpolation-result-function res)
                       (iota steps x-min (/ (- x-max x-min) steps)))))
    
    (define/public (draw-plot)
      (let ((dc (get-dc)))
        (for-each
         (lambda (method)
           (let ((f (method points)))
             (send dc draw-lines
                   (interpolation-result->points% f (steps)))))
         (send method-chooser get-methods))))

    (define (draw-points)
      (let* ((dc (get-dc))
             (old-brush (send dc get-brush)))
        (send dc set-brush "black" 'solid)
        (for-each
         (lambda (p)
           (let* ((pt (point->point% p))
                  (r (mark-radius))
                  (d (* r 2)))
             (send dc draw-ellipse
                   (- (send pt get-x) r)
                   (- (send pt get-y) r)
                   d d)))
         points)
        (send dc set-brush old-brush)))

    (define/public (clear-interpolation)
      (let ((dc (get-dc)))
        (clear-points)
        (send dc clear)))

    (define/public (draw-interpolation)
      (let ((dc (get-dc)))
        (if (send smoothing?-check-box get-value)
            (send dc set-smoothing 'smoothed)
            (send dc set-smoothing 'unsmoothed))
        (send dc clear))
      (when (not (null? points))
        (draw-plot)
        (draw-points)))

    (define/override (on-paint)
      (draw-interpolation))

    (define/override (on-event event)
      (when (send event button-down?)
        (set! points (append points (list (click->point event))))
        (draw-interpolation)))))

(define frame (new frame% 
                   [label "Interpolator"]
                   [width 500]
                   [height 500]))

(define method-choice (new method-choice%
                           [parent frame]
                           [label "Method"]
                           [choices '("Lagrange (λ)" 
                                      "Lagrange (matrix)"
                                      "Splines")]))

(define pad (new interpolation-pad%
                 [parent frame]
                 [x-min -10] [y-min -5]
                 [x-max 10] [y-max 5]
                 [method-chooser method-choice]))

(new button%
     [callback (lambda (b e) (send pad draw-interpolation))]
     [parent frame]
     [label "Interpolate"])
(new button%
     [callback (lambda (b e) (send pad clear-interpolation))]
     [parent frame]
     [label "Clear"])

(define smoothing?-check-box (new check-box%
                                  [parent frame]
                                  [label "Smoothing"]))
(send frame show #t)
