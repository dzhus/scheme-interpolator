#lang scheme

;;; Point structure

(require srfi/27)

(provide (rename-out [d:make-point make-point])
         point-x point-y
         point-dir
         vector->point
         endpoint->vector
         points->vector)

(define-struct point (x y dir))

(define (d:make-point x y [vec '#(0 0)])
  (make-point x y vec))

;; Calculate a distance between two points from E²
(define (point-distance p1 p2)
  (sqrt (+ (sqr (- (point-x p1) (point-x p2)))
           (sqr (- (point-y p1) (point-y p2))))))

;; Note that this is not a monomorphism
(define (endpoint->vector p)
  (vector (point-x p) (point-y p)))

;; Make a point with coordinates of a 2-dimensional vector
(define (vector->point vec)
  (d:make-point (vector-ref vec 0)
                (vector-ref vec 1)))

;; Make a vector which starts and ends at two given points
(define (points->vector p1 p2)
  (vector (- (point-x p2) (point-x p1))
          (- (point-y p2) (point-y p1))))
