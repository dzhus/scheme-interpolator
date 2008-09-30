#lang scheme/gui

(require srfi/1)

(require plot/extend)
(require plot)

(require "interpolate.ss")
(require "point.ss")

(define frame (new frame% [label "Interpolator"]))

(define (steps) 50)

(define plane%
  (class canvas%
    (inherit get-client-size get-dc)
    (init x-min y-min x-max y-max)
    (super-new)
    (define points '())
    (define (click->point event)
      (let-values (((width height) (get-client-size)))
        (let* ((click-x (send event get-x))
               (click-y (send event get-y))
               (x-factor (/ click-x width))
               (y-factor (/ click-y height))
               (x (+ x-min (* 1.0 x-factor (- x-max x-min))))
               (y (- y-max (* 1.0 y-factor (- y-max y-min)))))
          (make-point x y))))
    (define/public (clear-points)
      (set! points '()))

    (define (point->draw point)
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
    
    (define/public (draw-plot)
        (let ((f (interpolate points))
              (dc (get-dc))
              (pts (iota (steps) 0 (/ 10 (steps)))))
          (send dc draw-lines (map point->draw (function->grid f pts))))
        (clear-points)
        (flush-display))
    
    (define/override (on-event event)
      (when (send event button-down?)
        (set! points (append points (list (click->point event))))))))

(define canvas (new plane% 
                    [parent frame]
                    [x-min 0] [y-min 0]
                    [x-max 10] [y-max 10]))

(define run-button (new button%
                        [callback (lambda (b e) (send canvas draw-plot))]
                        [parent frame]
                        [label "Interpolate"]))

(send frame show #t)