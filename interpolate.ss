#lang scheme

;;; Different interpolation methods

(require srfi/1
         srfi/43
         "point.ss"
         "vectors.ss"
         "lambda-folds.ss"
         "matrix.ss"
         "linear-eq.ss"
         "shared.ss"
         "function.ss")

(provide function->grid
         lagrange-lambda-interpolation
         polynomial-interpolation
         spline-interpolation)

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
    (make-function
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
    (make-function
     'scalar
     (lambda (x)
       (vector-sum
        (vector-map
         (lambda (power coeff)
           (* (expt x power) coeff))
         coeffs))))))

;; Spline interpolation prototype
(define (spline-interpolation-segment points [t1 0] [t2 1])
  (let ((p1 (endpoint->vector (first points)))
        (p2 (endpoint->vector (last points)))
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
      (lambda (t)
        (add-vectors
         b1
         (add-vectors
          (vector-*-number b2 t)
          (add-vectors
           (vector-*-number b3 (sqr t))
           (vector-*-number b4 (expt t 3)))))))))

(define (spline-interpolation points)
  ;; Return upper bound for parameter of a spline interpolating
  ;; through given points (assuming lower is zero)
  (define (make-parameter-bound points) 1)
  ;; Given x value and list of intervals (with each interval being a
  ;; list of two values, too), return index of leftmost interval
  ;; enclosing x
  (define (enclosing-interval-index x intervals)
    (define (in-interval? x interval)
      (<= (first interval) x (second interval)))
    (list-index (lambda (i) (in-interval? x i)) intervals))
  ;; `'(1 2 3 4)` to `'((1 2) (2 3) (3 4))`
  (define (split-to-pairs l)
    (reverse! (fold (lambda (x r)
                      (cons (list (second (first r)) x) r))
                    (list (list (first l)
                                (second l)))
                    (drop l 2))))
  ;; `(a b c)` to `'(0 a a+b a+b+c)`
  (define (shift-accumulating list)
    (reverse! (fold (lambda (x r)
                      (cons (+ x (car r)) r))
                    '(0)
                    list)))
  (let* ((segments (split-to-pairs points))
         ;; $t_0, \ldots, t_k$
         (parameters (split-to-pairs
                      (shift-accumulating
                       (map (lambda (pair)
                              (make-parameter-bound pair))
                            segments))))
         ;; Interpolate on each segment
         (functions (map (lambda (points-pair t)
                           (let ((t1 (first t)) (t2 (second t)))
                             (spline-interpolation-segment points-pair t1 t2)))
                         segments parameters)))
    ;; Merge segments into one function
    (let ((spline (lambda (x)
                    (display (enclosing-interval-index x parameters))
                    ((list-ref functions
                                    (enclosing-interval-index x parameters)) x))))
      (make-function
       'vector spline (first (first parameters)) (last (last parameters))))))
