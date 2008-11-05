#lang scheme

(require srfi/1
         srfi/27
         srfi/43)
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(require "lambda-folds.ss"
         "shared.ss"
         "point.ss"
         "interpolate.ss"
         "function.ss"
         pyani-lib/matrix
         "linear-eq.ss")

(define test-epsilon 1e-10)

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
    "absmax-nonzero-index in column"
    (check-equal? (absmax-nonzero-vector-index '#( 0 1 2)) 2)
    (check-equal? (absmax-nonzero-vector-index '#( 11 -12 10)) 1)
    (check-equal? (absmax-nonzero-vector-index '#(0 7 999 -1000)) 3)
    (let ((n (add1 (random-integer 1000))))
      (check-equal? (absmax-nonzero-vector-index (list->vector (iota n))) (sub1 n)))
    (check-equal? (absmax-nonzero-vector-index (vector 1 -1 (random-integer 9) -2 3 3 4 4 10 -10 0)) 8))

   (test-case
    "Nondestructive vector items swapping (swap-vector-items)"
    (check-equal? (swap-vector-items 0 1 '#(1 2)) '#(2 1))
    (check-equal? (swap-vector-items 1 3 (list->vector (iota 5))) '#(0 3 2 1 4))
    (check-equal? (swap-vector-items 2 5 '#(9 17 2 -5 -5 0 0 1)) '#(9 17 0 -5 -5 2 0 1))
    (check-equal? (swap-vector-items 4 4 '#(0 0 0 0 1)) '#(0 0 0 0 1))
    (check-equal? (swap-vector-items 1 2 '#(3 3 3)) '#(3 3 3)))

   (test-case
    "vector-sum"
    (check-= (vector-sum '#(1 3 -13 32 7 4)) 34 0)
    (check-= (vector-sum '#(0 0 1 1 -1000 0 0 0 1000 -1)) 1 0)
    (check-= (vector-sum '#(1 2 3 4 5 5 4 3 2 1)) 30 0)
    (check-= (vector-sum '#(1 4 2 4)) (vector-sum '#(2 1 5 3)) 0)
    (check-= (vector-sum '#(-0 0 -0.0)) 0 0))))

(define-check (check-vectors v1 v2 epsilon)
  (vector= (lambda (x y) (check-= x y epsilon)) v1 v2))

(define-test-suite lineq-tests
  (test-case
   "Solve systems of linear equations"
   (check-vectors (solve-linear (matrix (row 1 3)
                                        (row 5 9))
                                (column 4 14))
                  '#(1 1)
                  test-epsilon)
   (check-vectors (solve-linear (matrix (row 1 2 3)
                                        (row 4 5 9)
                                        (row 9 -10 0))
                                (column 16 43 -50))
                  '#(0 5 2)
                  test-epsilon)
   (check-vectors (solve-linear (matrix (row -4 5 2 65)
                                        (row 2 -10 11 13)
                                        (row 3/7 -6 192 2)
                                        (row 6 0 13/8 0.5))
                                (column 580 262 27399/14 35.75))
                  '#(2.5 -3.0 10.0 9.0)
                  test-epsilon)
   (check-vectors (solve-linear (matrix (row 2 -9 5)
                                        (row 1.2 -5.3999 6)
                                        (row 1 -1 -7.5))
                                (column -4 0.6001 -8.5))
                  '#(0.0 1.0 1.0)
                  test-epsilon))

  (test-case
   "TDMA"
   (check-vectors (solve-tridiagonal (matrix (row 5 -1 0 0)
                                             (row 2 4.6 -1 0)
                                             (row 0 2 3.6 -0.8)
                                             (row 0 0 3 4.4))
                                     (column 2 3.3 2.6 7.2))
                  '#(0.52560 0.628 0.64 1.2)
                  test-epsilon)
   (check-vectors (solve-tridiagonal (matrix (row 1    2    0    0)
                                             (row 1.5  4    5    0)
                                             (row 0   -109 -1000 102)
                                             (row 0    0   -5.5  133.7))
                                     (column 3
                                             10.5
                                             -1107.98
                                             -4.163))
                  '#(1 1 1 0.01)
                  test-epsilon)))

(define-check (check-interpolation method function points epsilon)
  (let* ((grid
          (function->grid function points))
         (grid-interpolated
          (function->grid (function-lambda (method grid)) points)))
    (for-each
     (lambda (point point-interpolated)
       (check-= (point-y point)
                (point-y point-interpolated)
                epsilon
                (format "Interpolation ~s failed for ~s at x=~s"
                        method function (point-x point))))
     grid grid-interpolated)))

(define-test-suite interpolation-tests
  (test-case
   "Lagrange interpolation"
   (let* ((test-functions
           (list sin cos exp
                 (lambda (x) (- 5 (* x x)))
                 (lambda (x) (expt x 10))))
          (domains
           (map (lambda (x) (iota 100 -25 0.5)) test-functions)))
     (for-each
      (lambda (f domain)
        (check-interpolation lagrange-lambda-interpolation
                             f domain test-epsilon)
        (check-interpolation polynomial-interpolation
                             f domain 1e-1))
      test-functions domains))))

(exit (run-tests (test-suite "All tests"
                             shared-tests
                             interpolation-tests
                             lineq-tests)))
