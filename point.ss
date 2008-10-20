#lang scheme

(provide make-point 
         point-x point-y
         point-dx point-dy
         vector->point)

(define-struct point (x y [dx #:auto] [dy #:auto]) #:auto-value 0)

(define (point-distance p1 p2)
  (sqrt (+ (sqr (- (point-x p1) (point-x p2)))
           (sqr (- (point-y p1) (point-y p2))))))

(define (vector->point vec)
  (make-point (vector-ref vec 0)
              (vector-ref vec 1)))
