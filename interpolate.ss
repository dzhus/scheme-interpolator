#lang scheme

;;; Different interpolation methods

(require srfi/1
         srfi/43
         "point.ss"
         (prefix-in vec: "vectors.ss")
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

(define (spline-interpolation-segment points [t1 0] [t2 1])
  (let ((p1 (endpoint->vector (first points)))
        (p2 (endpoint->vector (last points)))
        (dp1 (point-dir (first points)))
        (dp2 (point-dir (last points))))
    (let ((b1 p1)
          (b2 dp1)
          (b3 (vec:-
               (vec:-
                (vec:*-number (vec:- p2 p1)
                              (/ 3 (sqr t2)))
                (vec:*-number dp1 (/ 2 t2)))
               (vec:/-number dp2 t2)))
          (b4 (vec:+
               (vec:+
                (vec:*-number (vec:- p1 p2)
                              (/ 2 (expt t2 3)))
                (vec:/-number dp1 (sqr t2)))
               (vec:/-number dp2 (sqr t2)))))
      (lambda (t)
        (vec:+
         b1
         (vec:+
          (vec:*-number b2 t)
          (vec:+
           (vec:*-number b3 (sqr t))
           (vec:*-number b4 (expt t 3)))))))))

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
  ;; Build a matrix of a system we'll use to find tangents at inner
  ;; points given a list of $t$ intervals
  (define (build-tangents-matrix parameters)
    (define (t-ref i) (list-ref parameters i))
    (let ((n (add1 (length parameters))))
      (build-matrix
       (lambda (i j)
         (cond ((or (= i 0) (= i (sub1 n))) (if (= i j) 1 0))
               ((= i (add1 j)) (second (t-ref i)))
               ((= i j) (* 2 (+ (first (t-ref i)) (second (t-ref i)))))
               ((= i (sub1 j)) (first (t-ref i)))
               (else 0)))
       n n)))
  ;; Given a list of parameter intervals and list of points, build
  ;; vector which is a right part in linear system used to find
  ;; tangents at inner points
  (define (build-tangents-vector parameters points)
    (define (t-ref i) (list-ref parameters i))
    (define (P-ref i) (list-ref points i))
    (let ((n (length points)))
      (build-vector
       n
       (lambda (i)
         (cond ((or (= i 0) (= i (sub1 n))) (point-dir (P-ref i)))
               (else
                (let ((t (t-ref i)))
                  (vec:*-number
                   (vec:+ (vec:*-number 
                           (vec:- (endpoint->vector (P-ref (add1 i)))
                                  (endpoint->vector (P-ref i)))
                           (sqr (first t)))
                          (vec:*-number 
                           (vec:- (endpoint->vector (P-ref i))
                                  (endpoint->vector (P-ref (sub1 i))))
                           (sqr (second t))))
                   (/ 3 (* (first t) (second t)))))))))))
  (define (populate-with-directions points tangents)
    (define (add-direction point dir)
      (make-point (point-x point) (point-y point) dir))
    (map add-direction points tangents))
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
  (let* ((parameters (split-to-pairs
                      (shift-accumulating
                       (map (lambda (pair)
                              (make-parameter-bound pair))
                            ;; We have to split points twice
                            (split-to-pairs points)))))
         (A (build-tangents-matrix parameters))
         (v (build-tangents-vector parameters points))
         (tangents (begin
                     (solve-by-components A v solve-tridiagonal)))
         (segments (split-to-pairs
                    (populate-with-directions points tangents)))
         ;; Interpolate on each segment
         (functions (map (lambda (points-pair t)
                           ;; @TODO On each segment lower bound for parameter is still zero!
                           (let ((t1 (first t)) (t2 (second t)))
                             (spline-interpolation-segment points-pair t1 t2)))
                         segments parameters)))
    ;; Merge segments into one function
    (define (spline x)
      (let* ((segment-number (enclosing-interval-index x parameters)))
             ;; @TODO Start from zero again
             ;(x (- x (first (list-ref parameters segment-number)))))
        ((list-ref functions segment-number) x)))
    (make-function
     'vector spline (first (first parameters)) (last (last parameters)))))
