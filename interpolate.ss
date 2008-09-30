#lang scheme

(require srfi/1)
(require "point.ss")
(require "lambda-folds.ss")
(require "shared.ss")

(provide interpolate function->grid)

(define (function->grid function grid)
  (map (lambda (x) (make-point x (function x))) grid))

(define (default-method)
  lagrange-lambda-interpolation)

(define (lagrange-lambda-interpolation points)
  (let ((k (length points)))
    (define (make-lagrange-fraction i)
      (define (make-lagrange-numer)
        (lambda-product
         (but-kth-item
          (map (lambda (l)
                 (lambda (x) (- x
                           (point-x (list-ref points l)))))
               (iota k))
          i)))
      (define (make-lagrange-denom)
        (lambda-product
         (but-kth-item
          (map (lambda (l)
                 (lambda (x) (- (point-x (list-ref points i))
                           (point-x (list-ref points l)))))
               (iota k))
          i)))
      (lambda (x)
        (/ ((make-lagrange-numer) x)
           ((make-lagrange-denom) x))))
    (lambda-sum
     (map
      (lambda (i)
        (lambda (x) (* (point-y (list-ref points i))
                  ((make-lagrange-fraction i) x))))
      (iota k)))))
  
(define (interpolate points [method (default-method)])
  (method points))

