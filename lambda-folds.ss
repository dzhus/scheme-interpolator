#lang scheme

(require srfi/1)
(provide lambda-sum lambda-product)

(define (lambda-fold compose initial list-of-functions)
  (fold (lambda (f1 f2) (lambda (x)
                     (compose (f1 x) (f2 x))))
        (lambda (x) initial)
        list-of-functions))

(define (lambda-sum list-of-functions)
  (lambda-fold + 0 list-of-functions))

(define (lambda-product list-of-functions)
  (lambda-fold * 1 list-of-functions))