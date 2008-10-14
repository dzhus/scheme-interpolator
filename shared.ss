#lang scheme

(require srfi/1)

(provide but-kth-item
         max-nonzero-index
         swap-items)

(define (but-kth-item list k)
  (append (take list k) (drop list (+ k 1))))

(define (max-nonzero-index vec)
  (fold
   (lambda (i prev)
     (if (> (vector-ref vec i) 
            (vector-ref vec prev))
         i
         prev))
   0
   (iota (vector-length vec))))

;;@ $l_1, \dotsc, l_i, l_j, \dotsc, l_n \rightarrow l_1, \dotsc, l_j, l_i, \dotsc, l_n$
(define (swap-items i j vec)
  (build-vector (vector-length vec)
                (lambda (n)
                  (cond ((= n i) (vector-ref vec j))
                        ((= n j) (vector-ref vec i))
                        (else (vector-ref vec n))))))
