#lang scheme

;;; Several functions for vector algebra

(require (prefix-in std: (only-in scheme/base + * /))
         srfi/43)

(provide + -
         *-number /-number)

;; @TODO: Rewrite this with generic `std-vector-op` using fixed point
;; combinator
(define (+ v . rest)
  (if (null? rest)
      v
      (apply + (append (list (vector-map (lambda (i x y) (std:+ x y)) v (first rest)))
                       (drop rest 1)))))

(define (*-number v s)
  (vector-map (lambda (i x) (* x s)) v))

(define (/-number v s)
  (*-number v (/ 1 s)))

(define-syntax -
  (syntax-rules ()
    [(- v1 v2) (+ v1 (*-number v2 -1))]))