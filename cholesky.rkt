#lang typed/racket
(require math)
(provide cholesky)

(define mref matrix-ref)

(: compute-diagonal-entry : ([Matrix Real]
                             [Matrix Real]
                             Natural
                             ->
                             Real))
(define (compute-diagonal-entry A L k)
  (define entry-squared
    (- (mref A k k)
       (for/sum: : Real
                 ([j (in-range 0 k)])
         (sqr (mref L k j)))))
  (if (< entry-squared 0)
      (error 'cholesky
             (string-append
              "This matrix isn't positive definite! "
              "Computing diagonal ~a, A_kk is ~a. "
              "k'th partial row is ~a. "
              "Entry squared values is ~a. ")
             k
             (mref A k k)
             (for/vector: : [Vectorof Real]
                          ([j (in-range 0 k)])
               (mref L k j))
             entry-squared)
      (sqrt entry-squared)))

(: build-cholesky-column : ([Matrix Real]
                            [Matrix Real]
                            Natural
                            Natural
                            ->
                            [Matrix Real]))
;; Given a positive definite matrix A and a partial cholesky factor of A such
;; that all columns less than k are already properly filled out
(define (build-cholesky-column A L k n)
  (define diagonal-entry
    (compute-diagonal-entry A L k))

  (for/array: #:shape (vector n 1)
              ([i (in-range 0 n)])
    : Real
    (cond [(< i k) 0]
          [(= i k) diagonal-entry]
          [else
           (* (/ 1 diagonal-entry)
              (- (mref A i k)
                 (for/sum: : Real
                           ([j (in-range 0 k)])
                   (* (mref L i j) (mref L k j)))))])))

(: cholesky : ((Matrix Real) -> (Matrix Real)))
(define (cholesky A)
  (define mref matrix-ref)
  (define n (matrix-num-rows A))

  (for*/fold: : [Matrix Real]
              ([L : [Matrix Real] (build-cholesky-column A
                                                         (make-matrix 0 0 0)
                                                         0
                                                         n)])
              ([k : Natural (in-range 1 n)])
    (matrix-augment (list L
                          (build-cholesky-column A L k n)))))
