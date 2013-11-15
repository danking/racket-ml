#lang typed/racket
(require math)
(provide cholesky)

(define mref matrix-ref)

(: compute-diagonal-entry : ([Matrix Number]
                             [Matrix Number]
                             Natural
                             ->
                             Number))
(define (compute-diagonal-entry A L k)
  (sqrt (- (mref A k k)
           (for/sum: : Number
                     ([j (in-range 0 k)])
             (sqr (mref L k j))))))

(: build-cholesky-column : ([Matrix Number]
                            [Matrix Number]
                            Natural
                            Natural
                            ->
                            [Matrix Number]))
;; Given a positive definite matrix A and a partial cholesky factor of A such
;; that all columns less than k are already properly filled out
(define (build-cholesky-column A L k n)
  (define diagonal-entry
    (compute-diagonal-entry A L k))

  (for/array: #:shape (vector n 1)
              ([i (in-range 0 n)])
    : Number
    (cond [(< i k) 0]
          [(= i k) diagonal-entry]
          [else
           (* (/ 1 diagonal-entry)
              (- (mref A i k)
                 (for/sum: : Number
                           ([j (in-range 0 k)])
                   (* (mref L i j) (mref L k j)))))])))

(: cholesky : ((Matrix Number) -> (Matrix Number)))
(define (cholesky A)
  (define mref matrix-ref)
  (define n (matrix-num-rows A))

  (for*/fold: : [Matrix Number]
              ([L : [Array Number] (build-cholesky-column A
                                                          (make-matrix 0 0 0)
                                                          0
                                                          n)])
              ([k : Natural (in-range 1 n)])
    (matrix-augment (list L
                          (build-cholesky-column A L k n)))))
