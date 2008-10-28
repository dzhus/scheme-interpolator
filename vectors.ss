#lang scheme

;;; Several functions for vector algebra

(require (prefix-in std: (only-in scheme/base + * /))
         srfi/43)

(provide + -
         *-number /-number)

;; TODO: arbitary arity
(define (+ v1 v2)
  (vector-map (lambda (i x y) (std:+ x y)) v1 v2))

;; (define-syntax +
;;   (syntax-rules ()
;;     [(+ v1 v2) (vector-map (lambda (i x y) (std:+ x y)) v1 v2)]
;;     [(+ v1 v2 ...) (+ (+ v1 v2) ...)]))

(define (*-number v s)
  (vector-map (lambda (i x) (* x s)) v))

(define (/-number v s)
  (*-number v (/ 1 s)))

(define-syntax -
  (syntax-rules ()
    [(- v1 v2) (+ v1 (*-number v2 -1))]))