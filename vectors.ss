#lang scheme

;;; Several functions for vector algebra

(require srfi/43)

(provide add-vectors sub-vectors
         vector-*-number vector-/-number)

;; TODO: arbitary arity
(define (add-vectors v1 v2)
  (vector-map (lambda (i x y) (+ x y)) v1 v2))

(define (vector-*-number v s)
  (vector-map (lambda (i x) (* x s)) v))

(define (vector-/-number v s)
  (vector-*-number v (/ 1 s)))

(define (sub-vectors v1 v2)
  (add-vectors v1 (vector-*-number v2 -1)))