#lang scheme/gui

(require srfi/1)

(require plot
         plot/extend)

(require "interpolate.ss"
         "point.ss")

(define (steps) 500)

(define (mark-radius) 2)

(define plane%
  (class canvas%
    (inherit get-client-size get-dc)
    ;; Bounds of currently visible plane frame
    (init x-min y-min x-max y-max)
    (super-new)

    (define points '())

    (define (click->point click-event)
      (let-values (((width height) (get-client-size)))
        (let* ((click-x (send click-event get-x))
               (click-y (send click-event get-y))
               (x-factor (/ click-x width))
               (y-factor (/ click-y height))
               (x (+ x-min (* 1.0 x-factor (- x-max x-min))))
               (y (- y-max (* 1.0 y-factor (- y-max y-min)))))
          (make-point x y))))

    (define/public (clear-points)
      (set! points '()))

    (define (point->point% point)
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

    (define (plot-function f steps)
      (let ((dc (get-dc))
            (pts (iota steps x-min (/ (- x-max x-min) steps))))
        (send dc draw-lines (map point->point% (function->grid f pts)))))

    (define/public (draw-plot)
      (for-each
       (lambda (method)
         (let ((f (method points)))
           (plot-function f (steps))))
       (chosen-interpolation-methods)))

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
                   (- (send pt get-x) r r)
                   (- (send pt get-y) r r)
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

(define canvas (new plane%
                    [parent frame]
                    [x-min -10] [y-min -5]
                    [x-max 10] [y-max 5]))

(new button%
     [callback (lambda (b e) (send canvas draw-interpolation))]
     [parent frame]
     [label "Interpolate"])
(new button%
     [callback (lambda (b e) (send canvas clear-interpolation))]
     [parent frame]
     [label "Clear"])

(define smoothing?-check-box (new check-box%
                                  [parent frame]
                                  [label "Smoothing"]))

(define method-choice (new choice%
                           [parent frame]
                           [label "Method"]
                           [choices '("Lagrange (λ)" 
                                      "Lagrange (matrix)")]))

(define (chosen-interpolation-methods)
  (list
   (let ((n (send method-choice get-selection)))
     (list-ref (list lagrange-lambda-interpolation
                     polynomial-interpolation)
               n))))

(send frame show #t)
