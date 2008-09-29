#lang scheme

(require srfi/1)
(require "point.ss")
(require "lambda-folds.ss")
(provide interpolate)

(define (interpolate points method)
  (define (make-lagrange-fraction i)
    (define (make-lagrange-numer)
      (lambda-product
       (but-kth-item
        (map (lambda (l)
               (lambda (x) (- x
                         (point-x (list-ref points l)))))
             (iota i))
        i)))
    (define (make-lagrange-denom)
      (lambda-product
       (but-kth-item
        (map (lambda (l)
               (lambda (x) (- (point-x (list-ref points i))
                         (point-x (list-ref points l)))))
             (iota i))
        i)))
    (lambda (x)
      (/ ((make-lagrange-numer) x)
         ((make-lagrange-denom) x))))
  (let ((k (length points)))
    (lambda-sum
     (map
      (lambda (i)
        (lambda (x) (* (point-y (list-ref points i))
                  ((make-lagrange-fraction i) x))))
      (iota k)))))