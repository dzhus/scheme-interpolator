#lang scheme

;;; Different interpolation methods

(require srfi/1
         srfi/43
         "point.ss"
         "lambda-folds.ss"
         "shared.ss"
         "function.ss"
         (planet wmfarr/simple-matrix:1:0/matrix)
         pyani-lib/matrix
         pyani-lib/vector
         pyani-lib/linear-eq)

(provide function->grid
         lagrange-lambda-interpolation
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

(define (spline-interpolation-segment points [t-max 1])
  (let ((p1 (endpoint->vector (first points)))
        (p2 (endpoint->vector (last points)))
        (dp1 (point-dir (first points)))
        (dp2 (point-dir (last points))))
    (let ((b1 p1)
          (b2 dp1)
          (b3 (vector-sub
               (vector-sub
                (vector-scale (vector-sub p2 p1) (/ 3 (sqr t-max)))
                (vector-scale dp1 (/ 2 t-max)))
               (vector-scale dp2 t-max)))
          (b4 (vectors-add
                (vector-scale (vector-sub p1 p2) (/ 2 (expt t-max 3)))
                (vector-scale dp1 (/ (sqr t-max)))
                (vector-scale dp2 (/ (sqr t-max))))))
      (lambda (t)
        (vectors-add
         b1
         (vector-scale b2 t) (vector-scale b3 (sqr t)) (vector-scale b4 (expt t 3)))))))

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
  ;; points given a list of $t$ upper bounds on each segment
  (define (build-tangents-matrix parameters)
    (define (t i) (list-ref parameters i))
    (let ((n (add1 (length parameters))))
      (build-matrix
       n n
       (lambda (i j)
         (cond ((or (= i 0) (= i (sub1 n))) (if (= i j) 1 0))
               ((= i (add1 j)) (t i))
               ((= i j) (* 2 (+ (t i) (t (sub1 i)))))
               ((= i (sub1 j)) (t (sub1 i)))
               (else 0))))))
  ;; Given a list of parameter upper bounds and list of points, build
  ;; vector which is a right part in linear system used to find
  ;; tangents at inner points
  (define (build-tangents-vector parameters points)
    (define (t i) (list-ref parameters i))
    (define (P-ref i) (list-ref points i))
    (let ((n (length points)))
      (build-vector
       n
       (lambda (i)
         (cond ((or (= i 0) (= i (sub1 n))) (point-dir (P-ref i)))
               (else
                (vector-scale
                 (vector-add
                  (vector-scale (vector-sub (endpoint->vector (P-ref (add1 i)))
                                            (endpoint->vector (P-ref i)))
                                (sqr (t (sub1 i))))
                  (vector-scale (vector-sub (endpoint->vector (P-ref i))
                                            (endpoint->vector (P-ref (sub1 i))))
                                (sqr (t i))))
                 (/ 3 (* (t i) (t (sub1 i)))))))))))
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
  ;; `((a b) (c d))` to `((a b) (b+c b+d))`
  (define (stack-intervals list)
    (drop (reverse! (fold (lambda (x r)
                            (cons (map (lambda (p) (+ p (second (car r)))) x) r))
                          '((0 0))
                          list))
          1))
  (let* ((parameters (map make-parameter-bound
                          ;; (We'll have to split points twice)
                          (split-to-pairs points)))
         (A (build-tangents-matrix parameters))
         (v (build-tangents-vector parameters points))
         (tangents (solve-by-components A v solve-tridiagonal))
         (segments (split-to-pairs
                    (populate-with-directions points tangents)))
         ;; Interpolate on each segment
         (functions (map (lambda (points-pair t)
                           (spline-interpolation-segment points-pair t))
                         segments parameters)))
    ;; Merge segments into one function
    (let ((shifted-parameters (stack-intervals
                               (map (lambda (t) (list 0 t)) parameters))))
      (define (spline x)
        ;; We treat parameter as continuous along the whole spline in
        ;; order to choose corresponding segment. On each segment
        ;; lower bound for argument is still zero!
        (let* ((segment-number (enclosing-interval-index x shifted-parameters))
               (x (- x (first (list-ref shifted-parameters segment-number)))))
          ((list-ref functions segment-number) x)))
      (make-function
       'vector spline
       (first (first shifted-parameters))
       (last (last shifted-parameters))))))
