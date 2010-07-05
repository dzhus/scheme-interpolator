#lang scheme

(require srfi/1
         srfi/27
         srfi/43)

(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(require "lambda-folds.ss"
         "point.ss"
         "interpolate.ss"
         "function.ss")

(define test-epsilon 1e-10)

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
                             interpolation-tests)))
