#lang scheme

(require srfi/27)

(provide (rename-out [d:make-point make-point])
         point-x point-y
         point-dir
         vector->point
         point->vector)

(define-struct point (x y dir))

(define (d:make-point x y [vec '#(-5 5)])
  (make-point x y (vector (- (random-integer 20) 10)
                          (- (random-integer 20) 10))))

(define (point-distance p1 p2)
  (sqrt (+ (sqr (- (point-x p1) (point-x p2)))
           (sqr (- (point-y p1) (point-y p2))))))

;; Note that this is not a monomorphism
(define (point->vector p)
  (vector (point-x p) (point-y p)))

(define (vector->point vec)
  (d:make-point (vector-ref vec 0)
                (vector-ref vec 1)))
