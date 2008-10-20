#lang scheme

(require srfi/1
         srfi/43
         "point.ss"
         "vectors.ss"
         "lambda-folds.ss"
         "matrix.ss"
         "gauss.ss"
         "shared.ss")

(provide function->grid
         lagrange-lambda-interpolation
         polynomial-interpolation
         spline-interpolation
         interpolation-result-type
         interpolation-result-function)

(define-struct interpolation-result (type function))

(define (function->grid function domain)
  (map (lambda (x) (make-point x (function x))) domain))

;; Build Lagrange polynomial on the fly
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
    (make-interpolation-result
     'scalar
     (lambda-sum
      (map
       (lambda (i)
         (lambda (x) (* (point-y (list-ref points i))
                        ((make-lagrange-fraction i) x))))
       (iota k))))))

;; Build interpolation polynomial solving a system of linear equations
;; with Vandermonde matrix (heavily prone to precision errors as usual
;; Gauss elimination is used in `solve-linear`)
(define (polynomial-interpolation points)
  (let* ((k (length points))
         (matrix (rows->matrix
                  (map
                   (lambda (point)
                     (let ((x (point-x point)))
                       (list->row
                        (map
                         (lambda (power)
                           (expt x power))
                         (iota k)))))
                   points)))
         (right-column (list->column
                        (map point-y points)))
         (coeffs (solve-linear matrix right-column)))
    (make-interpolation-result
     'scalar
     (lambda (x)
       (vector-sum
        (vector-map
         (lambda (power coeff)
           (* (expt x power) coeff))
         coeffs))))))

;; Spline interpolation prototype
(define (spline-interpolation points [t1 0] [t2 1])
  (let ((p1 (point->vector (first points)))
        (p2 (point->vector (last points)))
        (dp1 (point-dir (first points)))
        (dp2 (point-dir (last points))))
    (let ((b1 p1)
          (b2 dp1)
          (b3 (sub-vectors
               (sub-vectors
                (vector-*-number (sub-vectors p2 p1)
                                 (/ 3 (sqr t2)))
                (vector-*-number dp1 (/ 2 t2)))
               (vector-/-number dp2 t2)))
          (b4 (add-vectors
               (add-vectors
                (vector-*-number (sub-vectors p1 p2)
                                 (/ 2 (expt t2 3)))
                (vector-/-number dp1 (sqr t2)))
               (vector-/-number dp2 (sqr t2)))))
  (make-interpolation-result
   'vector
   (lambda (t)
     (add-vectors
      b1
      (add-vectors
       (vector-*-number b2 t)
       (add-vectors
        (vector-*-number b3 (sqr t))
        (vector-*-number b4 (expt t 3))))))))))