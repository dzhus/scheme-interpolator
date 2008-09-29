#lang scheme

(provide make-point point-x point-y)

(define-values (struct:point make-point point? point-ref point-set!)
  (make-struct-type 'point #f 2 0))

(define point-x (make-struct-field-accessor point-ref 0))
(define point-y (make-struct-field-accessor point-ref 1))
