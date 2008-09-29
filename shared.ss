#lang scheme

(require srfi/1)

(provide but-kth-item)

(define (but-kth-item list k)
  (append (take list k) (drop list (+ k 1))))