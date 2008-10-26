#lang scheme

(require srfi/1
         srfi/43
         "matrix.ss"
         "shared.ss")
(require mzlib/trace)

(provide solve-linear
         solve-tridiagonal)

;; Solve a system of linear equations given its matrix A and right
;; vector v, given A is _invertible_
(define (solve-linear A v)
  ;; Get the top left coefficient of an augmented matrix
  (define (top-left equations)
    (matrix-item equations 0 0))
  ;; Get the top right coefficient of an augmented matrix
  (define (top-right equations)
    (matrix-item equations 0 (sub1 (row-length (first-row equations)))))
  (define (column-reduce equations)
    (let ((first-equation (first-row equations))
          (top-left (top-left equations)))
      (matrix-but-first-column
       (matrix-map
        (lambda (i j a)
          (- a
             (/ (* (row-item first-equation j)
                   ;; We skip the first row, so add 1 to row index
                   (matrix-item equations (add1 i) 0))
                top-left)))
        (matrix-but-first-row equations)))))
  ;; Given an augmented matrix with one equation of n variables and a
  ;; vector with values of (n-1) of them, make a system of the only
  ;; trivial equation ax=c
  (define (make-trivial equations subsolution)
    (let ((coeffs-row (row-but-first
                       (row-but-last (first-row equations)))))
      (matrix
       (row (top-left equations)
            (- (top-right equations)
               (vector-sum
                (vector-map (lambda (i x)
                              (* x (row-item coeffs-row i)))
                            subsolution)))))))
  (define (trivial? equations)
    (and (= (matrix-size equations) 1)
         (= (row-length (first-row equations)) 2)))
  ;; Solve a system of linear equations given its augmented matrix
  (define (solve-equations equations)
    (if (trivial? equations)
        ;; Solve trivial equation (ax=c) immediately
        (if (= (top-left equations) 0)
            (error "No solution: dependant rows")
            (vector (/ (top-right equations)
                       (top-left equations))))
        ;; Choose maximum element in first column and make that row a
        ;; new top to avoid accidental division by zero (non-zero
        ;; element always exists as A is invertible)
        (let* ((leading-row (max-nonzero-column-index
                             (first-column equations)))
               (equations (swap-matrix-rows 0 leading-row equations)))
          (if (= (top-left equations) 0)
              (error "No solution: dependant columns")
              (let ((subsolution (solve-equations
                                  (column-reduce equations))))
                (vector-append
                 ;; Solve an equation with only 1 variable (backward
                 ;; pass)
                 (solve-equations (make-trivial equations subsolution))
                 subsolution))))))
  (let ((augmented (add-column A v)))
    (solve-equations augmented)))

;; Solve system of equations with tridiagonal matrix
(define (solve-tridiagonal A v)
  (let ([k (matrix-size v)])
    (define (a i) (matrix-item A i (sub1 i)))
    (define (b i) (matrix-item A i i))
    ;; Phantom $c_{k-1}=0$ so we don't need extra conditionals in unfold
    ;; below
    (define (c i) (matrix-item A i (add1 i)))
    (define (d i) (vector-ref v i))
    (let ([alpha-beta (vector-unfold-right
                       (lambda (i alpha beta)
                         (let ([i (- k i)])
                           (if (< i k)
                               (let ([gamma (+ (* (a i) alpha) (b i))])
                                 (values (cons alpha beta)
                                         ;; Don't calculate alpha on
                                         ;; last step (not needed
                                         ;; anyways)
                                         (if (< i (sub1 k))
                                             (/ (- (c i)) gamma)
                                             #f)
                                         (/ (- (d i) (* (a i) beta)) gamma)))
                               ;; We don't want to calculate anything
                               ;; beyond folding limit
                               (values (cons alpha beta) #f #f))))
                       k
                       (/ (- (c 0)) (b 0))
                       (/ (d 0) (b 0)))])
      (define (alpha i) (car (vector-ref alpha-beta i)))
      (define (beta i) (cdr (vector-ref alpha-beta i)))
      (trace alpha beta)
      ;; alpha-beta is sorted by index in _descending_ order, so
      ;; `(beta 0)` is actually the last calculated beta
      (vector-unfold (lambda (i x)
                       (let* ([i (add1 i)])
                         ;; Yet again calculations beyond folding
                         ;; bound are unnecessary
                         (if (< i k)
                             (values x (+ (* (alpha i) x) (beta i)))
                             (values x #f))))
                     k
                     (beta 0)))))
