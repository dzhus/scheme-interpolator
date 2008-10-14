#lang scheme

(require srfi/1 srfi/27)
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(require "lambda-folds.ss"
         "shared.ss"
         "point.ss"
         "interpolate.ss")

(define test-epsilon 0)

(define-test-suite shared-tests
  (test-case
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
             (check-equal? (sum2 x) (+ (f1 x) (f1 x) (f1 x)))
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

  (test-case
   "Common functions"
   (test-case
    "but-kth-item"
    (check-equal? (but-kth-item '(5) 0) '())
    (check-equal? (but-kth-item '(1 2 3) 0) '(2 3))
    (check-equal? (but-kth-item '(1 2 3) 1) '(1 3))
    (check-equal? (but-kth-item '(1 2 3) 2) '(1 2)))

   (test-case
    "check-equal?"
    (check-equal? (max-nonzero-index (vector 0 1 2)) 2)
    (check-equal? (max-nonzero-index (vector 11 -12 10)) 0)
    (check-equal? (max-nonzero-index (vector 0 7 9 -1000)) 2)
    (let ((n (add1 (random-integer 1000))))
      (check-equal? (max-nonzero-index (list->vector (iota n))) (sub1 n)))
    (check-equal? (max-nonzero-index (vector 1 -1 (random-integer 9) -2 3 3 4 4 4 10 0)) 9))

   (test-case
    "Nondestructive vector items swapping (swap-items)"
    (check-equal? (swap-items 0 1 (vector 1 2)) (vector 2 1))
    (check-equal? (swap-items 1 3 (list->vector (iota 5))) (vector 0 3 2 1 4))
    (check-equal? (swap-items 2 5 (vector 9 17 2 -5 -5 0 0 1)) (vector 9 17 0 -5 -5 2 0 1))
    (check-equal? (swap-items 4 4 (vector 0 0 0 0 1)) (vector 0 0 0 0 1))
    (check-equal? (swap-items 1 2 (vector 3 3 3)) (vector 3 3 3)))))

(define-test-suite matrix-tests
  (test-case
   "Matrix operations"
   (check-true #f)))

(define-test-suite gauss-tests
  (test-case
   "Solve systems of linear equations"
   (check-true #f)))

(define-test-suite interpolation-tests
  (test-case
   "Lagrange interpolation"
   (let* ((test-functions
           (list sin cos exp
                 (lambda (x) (- 5 (* x x)))
                 (lambda (x) (expt x 10))))
          (intervals
           (map (lambda (x) (iota 100 -50 0.5)) test-functions))
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
     grids intervals))))

(exit (run-tests (test-suite "All tests"
                             shared-tests
                             matrix-tests
                             gauss-tests
                             interpolation-tests)))
