#lang scheme/gui

(require srfi/1)

(require plot
         plot/extend)

(require "interpolate.ss"
         "point.ss")

(define (steps) 500)

(define (mark-radius) 2)

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

(define plane-frame-interface
  (interface ()
    point->point% click->point))

(define plane-frame-mixin
  (mixin
      (window<%>)
      (plane-frame-interface)
    
      (inherit get-client-size)
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

(define points-pad-interface
  (interface ()
    add-point clear-points set-kth-point! points-updated draw-point))

(define points-pad-mixin
  (mixin
      (plane-frame-interface canvas<%>)
      (points-pad-interface)
    
      (inherit click->point point->point% get-dc)

      (super-new)

      (field (points '()))

      (define/public (add-point p)
        (set! points (append points (list p)))
        (points-updated))

      (define/public (clear-points)
        (set! points '())
        (points-updated))

      (define/public (set-kth-point! k p)
        (set! points
              (append
               (take points k)
               (list p)
               (drop points (add1 k))))
        (points-updated))

      (define/pubment (points-updated)
        (inner points points-updated))

      (define/public (draw-point p)
        (let* ((dc (get-dc))
               (old-brush (send dc get-brush)))
          (send dc set-brush "black" 'solid)
          (let* ((pt (point->point% p))
                 (r (mark-radius))
                 (d (* r 2)))
            (send dc draw-ellipse
                  (- (send pt get-x) r)
                  (- (send pt get-y) r)
                  d d))
          (send dc set-brush old-brush)))

      (define/override (on-event event)
        (cond ((send event dragging?)
               (let* ((last-point (last points))
                      (new-direction (points->vector last-point
                                                     (click->point event)))
                      (new-point (make-point (point-x last-point)
                                             (point-y last-point)
                                             new-direction)))
                 (set-kth-point! (sub1 (length points)) new-point)))
              ((send event button-down?)
               (add-point (click->point event)))))))

(define interpolating-frame-interface
  (interface ()
    draw-interpolation-result))

(define interpolating-frame-mixin
  (mixin
      (plane-frame-interface canvas<%>)
      (interpolating-frame-interface)
    
      (inherit point->point% get-dc)
      (inherit-field x-min x-max)
      
      (super-new)

      (define (interpolation-result->points% res steps)
        ;; Scalar functions f(x)
        (define (map-scalar f range)
          (map (lambda (p) (point->point% p)) ; we can't write `point->point%` here
               (function->grid f range)))
        ;; Vector functions r(t)
        (define (map-vector f range)
          (map (lambda (v) (point->point% (vector->point v)))
               (map f range)))
        (let ((res-type (interpolation-result-type res)))
          (cond ((eq? 'vector res-type)
                 (map-vector (interpolation-result-function res)
                             (append
                              (map (lambda (t) (/ t steps)) (iota steps))
                              (list 1))))
                ((eq? 'scalar res-type)
                 (map-scalar (interpolation-result-function res)
                             (iota steps x-min (/ (- x-max x-min) steps)))))))

      (define/public (draw-interpolation-result res [steps 500])
        (let ((dc (get-dc)))
          (send dc draw-lines
                (interpolation-result->points% res steps))))))

(define interpolation-workspace-interface
  (interface ()
    draw-interpolation-plots
    redraw-interpolation
    clear-interpolation))

(define interpolation-workspace-mixin
  (mixin
      (points-pad-interface
       interpolating-frame-interface
       canvas<%>)
      (interpolation-workspace-interface)

      (inherit get-dc clear-points draw-point draw-interpolation-result)
      (inherit-field points)
      (init-field method-chooser)
      (super-new)

      (define/public (draw-interpolation-plots points methods)
        (for-each
         (lambda (method)
           (draw-interpolation-result (method points)))
         methods))

      (define/public (redraw-interpolation)
        (let ((dc (get-dc)))
          (when (not (null? points))
            (send dc clear)
            (draw-interpolation-plots points
                                      (send method-chooser get-methods))
            (for-each (lambda (p) (draw-point p)) points))))
      
      (define/public (clear-interpolation)
        (let ((dc (get-dc)))
          (clear-points)
          (send dc clear)))

      (define/override (on-paint)
        (redraw-interpolation))

      (define/augment (points-updated)
        (redraw-interpolation))))

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

(define pad (new (interpolation-workspace-mixin
                  (interpolating-frame-mixin
                   (points-pad-mixin
                    (plane-frame-mixin
                     canvas%))))
                 [parent frame]
                 [x-min -10] [y-min -5]
                 [x-max 10] [y-max 5]
                 [method-chooser method-choice]))

(send (send pad get-dc) set-smoothing 'smoothed)

(new button%
     [callback (lambda (b e) (send pad redraw-interpolation))]
     [parent frame]
     [label "Interpolate"])
(new button%
     [callback (lambda (b e) (send pad clear-interpolation))]
     [parent frame]
     [label "Clear"])

(send frame show #t)
