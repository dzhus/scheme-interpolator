#lang scheme

;;; Function structure

(provide (rename-out [d:make-function make-function])
         function-type
         function-lambda
         function-min-arg
         function-max-arg)

(define-struct function (type lambda min-arg max-arg))

(define (d:make-function type lambda [min-arg -inf.0] [max-arg +inf.0])
  (make-function type lambda
                 min-arg max-arg))
