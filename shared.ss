#lang scheme

(require srfi/1
         srfi/43)

(provide but-kth-item
         absmax-nonzero-vector-index)


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
