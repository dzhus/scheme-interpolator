#lang scheme

(require srfi/1
         srfi/43)

(provide but-kth-item
         absmax-nonzero-vector-index
         swap-vector-items
         vector-sum)

(define (but-kth-item list k)
  (append (take list k) (drop list (+ k 1))))

(define (absmax-nonzero-vector-index vec)
  (fold
   (lambda (i prev)
     (if (> (abs (vector-ref vec i))
            (abs (vector-ref vec prev)))
         i
         prev))
   0
   (iota (vector-length vec))))

(define (swap-vector-items i j vec)
  (build-vector (vector-length vec)
                (lambda (n)
                  (cond ((= n i) (vector-ref vec j))
                        ((= n j) (vector-ref vec i))
                        (else (vector-ref vec n))))))

(define (vector-sum vec)
  (vector-fold (lambda (i sum x) (+ sum x)) 0 vec))
