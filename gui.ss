#lang scheme/gui

(require srfi/1)

(require plot
         plot/extend)

(require "interpolate.ss"
         "point.ss"
         "function.ss")

(define (samples) 500)

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
    point->drawable click->point))

;; A rectangular region of cartesian plane with methods for
;; interaction and visualization
(define plane-frame-mixin
  (mixin
      (window<%>)
      (plane-frame-interface)
    
      (inherit get-client-size)
      ;; Boundaries of region
      (init-field x-min y-min x-max y-max)

      (super-new)

      ;; Convert a point of the plane to a drawable `point%` instance
      (define/public (point->drawable point)
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

      ;; Translate a click on area to a point of the plane (respecting
      ;; client size and region boundaries)
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

;; A plane-frame user may pick points on interactively
(define points-pad-mixin
  (mixin
      (plane-frame-interface canvas<%>)
      (points-pad-interface)
    
      (inherit click->point point->drawable get-dc)

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

      ;; Subject to augmentation in subclassing classes, a method
      ;; called when calls to `add-point`, `clear-points` and
      ;; `set-kth-point!` occur
      (define/pubment (points-updated)
        (inner points points-updated))

      (define/public (draw-point p)
        (let* ((dc (get-dc))
               (old-brush (send dc get-brush)))
          (send dc set-brush "black" 'solid)
          (let* ((pt (point->drawable p))
                 (r (mark-radius))
                 (d (* r 2)))
            (send dc draw-ellipse
                  (- (send pt get-x) r)
                  (- (send pt get-y) r)
                  d d))
          (send dc set-brush old-brush)))

      ;; A click on area adds a new point, dragging adds a point at
      ;; position where dragging began with vector pointing out from
      ;; this point to where dragging ended
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

(define plotting-frame-interface
  (interface ()
    plot-function))

;; A plane-frame which plots different types of functions
(define plotting-frame-mixin
  (mixin
      (plane-frame-interface canvas<%>)
      (plotting-frame-interface)
    
      (inherit point->drawable get-dc)
      (inherit-field x-min x-max)
      
      (super-new)

      ;; Generate a list of drawable `point%` instances given a
      ;; function structure and sampling rate
      (define (function->drawable fun samples)
        ;; Scalar functions f(x)
        (define (map-scalar f range)
          (map (lambda (p) (point->drawable p))
               (function->grid f range)))
        ;; Vector functions r(t)
        (define (map-vector f range)
          (map (lambda (v) (point->drawable (vector->point v)))
               (map f range)))
        (let ((fun-type (function-type fun)))
          (cond ((eq? 'vector fun-type)
                 (let ((min-arg (function-min-arg fun))
                       (max-arg (function-max-arg fun)))
                   (map-vector (function-lambda fun)
                               (append
                                (iota samples min-arg (/ max-arg samples))
                                (list max-arg)))))
                ((eq? 'scalar fun-type)
                 (map-scalar (function-lambda fun)
                             (iota samples x-min (/ (- x-max x-min) samples)))))))

      (define/public (plot-function fun samples)
        (let ((dc (get-dc)))
          (send dc draw-lines
                (function->drawable fun samples))))))

(define interpolation-workspace-interface
  (interface ()
    draw-interpolation-plots
    redraw-interpolation))

;; A plane-frame where user may pick points and interpolate them using
;; different methods
(define interpolation-workspace-mixin
  (mixin
      (points-pad-interface
       plotting-frame-interface
       canvas<%>)
      (interpolation-workspace-interface)

      (inherit get-dc clear-points draw-point plot-function)
      (inherit-field points)
      (init-field method-chooser)
      (super-new)

      ;; For each method in given list, plot a function which
      ;; interpolates list of points
      (define/public (draw-interpolation-plots points methods)
        (for-each
         (lambda (method)
           (plot-function (method points) (samples)))
         methods))

      ;; Mark all points in the frame and interpolate them using every
      ;; method from a list returned by `method-chooser`
      (define/public (redraw-interpolation)
        (let ((dc (get-dc)))
          (send dc clear)
          (when (> (length points) 1)
            (draw-interpolation-plots points
                                      (send method-chooser get-methods))
            (for-each (lambda (p) (draw-point p)) points))))
      

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
                  (plotting-frame-mixin
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
     [callback (lambda (b e) (send pad clear-points))]
     [parent frame]
     [label "Clear"])

(send frame show #t)
