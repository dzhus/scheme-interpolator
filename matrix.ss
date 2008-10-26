#lang scheme

(require srfi/1
         srfi/43
         "shared.ss")

(provide matrix row column
	 rows->matrix list->row list->column
         matrix-row row-item column-item matrix-item
         matrix-size row-length column-length
         row-drop-right row-drop-left
         first-row row-but-first row-but-last
         matrix-but-first-row
         matrix-but-first-column
         first-column add-column
         absmax-nonzero-column-index
         swap-matrix-rows
         matrix-map
         build-matrix)

(define matrix vector)
(define row vector)
(define column vector)

(define rows->matrix list->vector)
(define list->row list->vector)
(define list->column list->vector)

(define matrix-row vector-ref)
(define row-item vector-ref)
(define column-item vector-ref)
(define (matrix-item matrix i j)
  (row-item (matrix-row matrix i) j))

(define row-length vector-length)
(define column-length vector-length)
(define matrix-size vector-length)

(define (first-row matrix)
  (vector-ref matrix 0))

(define (row-drop-right row n)
  (vector-copy row 0 (- (vector-length row) n)))

(define (row-drop-left row n)
  (vector-copy row n))

(define (row-but-first row)
  (row-drop-left row 1))

(define (row-but-last row)
  (row-drop-right row 1))

(define matrix-but-first-row row-but-first)

(define (first-column matrix)
  (vector-map (lambda (i row)
                (row-item row 0))
              matrix))

(define (matrix-but-first-column matrix)
  (vector-map (lambda (i row)
                (row-but-first row))
              matrix))

(define (add-column matrix column)
  (vector-map (lambda (i matrix-row column-item)
                (vector-append matrix-row (vector column-item)))
              matrix column))

(define absmax-nonzero-column-index absmax-nonzero-vector-index)
(define swap-matrix-rows swap-vector-items)

(define (matrix-map proc matrix)
  (vector-map (lambda (i row)
                (vector-map
                 (lambda (j item)
                   (proc i j item))
                 row))
              matrix))

(define (build-matrix proc rows columns)
  (matrix-map (lambda (i j e) (proc i j))
              (make-vector rows (make-vector columns))))

  