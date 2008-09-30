#lang scheme

(require srfi/1 srfi/27)
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(require "lambda-folds.ss")
(require "shared.ss")
(require "point.ss")
(require "interpolate.ss")

(define test-epsilon 0)

(define-test-suite all-tests
  (test-suite
   "Lambda-folds"
   (let ((k1 (add1 (random-integer 1000)))
         (k2 (random-integer 5)))
     (let ((f1 (lambda (x) (* k1 x)))
           (f2 (lambda (x) (* k2 x)))
           (f3 (lambda (x) (/ x k1)))
           (f4 (lambda (x) (* (- k2) x))))
     (test-case
      "Lambda-sum"
      (let ((sum1 (lambda-sum (list f1 f2 f3)))
            (sum2 (lambda-sum (list f1 f1 f1)))
            (sum3 (lambda-sum (list f2 f4)))
            (sum4 (lambda-sum '())))
        (for-each
         (lambda (x)
           (check-equal? (sum1 x) (+ (f1 x) (f2 x) (f3 x)))
           (check-equal? (sum2 x) (* (f1 x) (f1 x) (f1 x)))
           (check-equal? (sum3 x) 0)
           (check-equal? (sum4 x) 0))
         (iota 10 -5 1))))
     (test-case
      "Lambda-product"
      (let ((prod1 (lambda-product (list f1 f2 f3)))
            (prod2 (lambda-product (list f1 f1 f1)))
            (prod3 (lambda-product (list f2 f4)))
            (prod4 (lambda-product '())))
        (for-each
         (lambda (x)
           (check-equal? (prod1 x) (* (f1 x) (f2 x) (f3 x)))
           (check-equal? (prod2 x) (* (f1 x) (f1 x) (f1 x)))
           (check-equal? (prod3 x) (* (f2 x) (f4 x)))
           (check-equal? (prod4 x) 1))
         (iota 10 -5 1)))))))

  (test-suite
   "Common functions"
   (let ((list1 '(5))
         (list2 '(1 2 3)))
     (test-case
      "but-kth-item"
      (check-equal? (but-kth-item list1 0) '())
      (check-equal? (but-kth-item list2 0) '(2 3))
      (check-equal? (but-kth-item list2 1) '(1 3))
      (check-equal? (but-kth-item list2 2) '(1 2)))))

  (test-suite
   "Lagrange interpolation"
   (test-begin
    (let* ((test-functions
            (list sin cos exp
                  (lambda (x) (- 5 (* x x)))
                  (lambda (x) (expt x 10))))
           (intervals
            (map (lambda (x) (iota 200 -100 0.5)) test-functions))
           (grids
            (map function->grid test-functions intervals)))
      (for-each
       (lambda (grid interval)
         (let ((grid-interpolated
                (function->grid (interpolate grid) interval)))
           (for-each
            (lambda (point point-interpolated)
              (check-= (point-y point) (point-y point-interpolated)
                       test-epsilon))
            grid grid-interpolated)))        
       grids intervals)))))

(run-tests all-tests)