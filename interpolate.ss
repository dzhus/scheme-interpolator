#lang scheme

(require srfi/1
         srfi/43
         "point.ss"
         "lambda-folds.ss"
         "matrix.ss"
         "gauss.ss"
         "shared.ss")

(provide function->grid
         lagrange-lambda-interpolation
         polynomial-interpolation)

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
    (lambda-sum
     (map
      (lambda (i)
        (lambda (x) (* (point-y (list-ref points i))
                  ((make-lagrange-fraction i) x))))
      (iota k)))))

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
    (lambda (x)
      (vector-sum
       (vector-map
        (lambda (power coeff)
          (* (expt x power) coeff))
        coeffs)))))
