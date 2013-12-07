#lang typed/racket

(require math
         racket-ml
         (only-in racket/flonum flexpt)
         )

(provide multivariate-normal-dist
         pdf-multivariate-normal
         sample-multivariate-normal
         )

(: multivariate-normal-dist : ([Matrix Real]
                               [Matrix Real]
                               ->
                               (distribution [Matrix Real]
                                             [Matrix Flonum])))
(define (multivariate-normal-dist mean covariance)
  (distribution (pdf-multivariate-normal mean covariance)
                (sample-multivariate-normal mean covariance)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note that the covariance matrices here must be nonnegative-definite in order
;; to produce true multivariate-normal distributions. I do not check this
;; property (primarily because there doesn't currently exist an eigenvalue
;; function and nonnegative-definite can be checked mostly easily with the
;; eigenvalues).

(: pdf-multivariate-normal : ([Matrix Real]
                              [Matrix Real]
                              ->
                              (PDF [Matrix Real])))
(define (pdf-multivariate-normal mean covariance)
  (define det (fl (matrix-determinant covariance)))
  (define k (fl (square-matrix-size covariance)))
  (define 2pi^-k (flexpt (/ 1 (* 2 pi)) k))
  (define coefficient
    (if (or (< 2pi^-k 0) (<= det 0))
        (begin
          (displayln covariance)
          (error 'pdf-multivariate-normal
                 (string-append
                  "I'll bet you the covariance matrix (should be printed above) "
                  "isn't nonngetaive-definite. Tried to take the square root of "
                  "a negative number.")))
        (* (sqrt 2pi^-k)
           (sqrt (/ 1.0 det)))))
  (: exponent : [Matrix Real] -> Flonum)
  (define (exponent x)
    (define difference (matrix- x mean))

    (- (* 0.5
          (fl (1x1-matrix->scalar (matrix* (matrix-transpose difference)
                                           (matrix-inverse covariance)
                                           difference))))))

  (: pdf : (PDF [Matrix Real]))
  (define (pdf x [log? #f])
    (if log?
        (+ (log coefficient) (exponent x))
        (* coefficient (exp (exponent x)))))

  pdf)

(: sample-multivariate-normal : ([Matrix Real]
                                 [Matrix Real]
                                 ->
                                 (Sample [Matrix Flonum])))
(define (sample-multivariate-normal mean covariance)
  (define L (array->flarray (cholesky covariance)))

  (: sample : (Sample [Matrix Flonum]))
  (define (sample [samples #f])
    (if samples
        (for/list: : [Listof [Matrix Flonum]]
                   ([i (in-range 0 samples)])
          (matrix+ mean (matrix* L (base-mvn (square-matrix-size covariance)))))
        (matrix+ mean (matrix* L (base-mvn (square-matrix-size covariance))))))

  sample)

(: base-mvn : (Natural -> [Matrix Flonum]))
(define (base-mvn n)
  (for/matrix: n 1
               ([i (in-range 0 n)])
    : Flonum
    (sample (normal-dist 0 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

(: 1x1-matrix->scalar : (All (A) ([Matrix A] -> A)))
;; takes a 1x1 matrix and demotes the value to a scalar
(define (1x1-matrix->scalar m)
  (if (= 1 (square-matrix-size m))
      (matrix-ref m 0 0)
      (error '1x1-matrix->scalar
             "Matrix is not a 1x1, actually is ~ax~a."
             (square-matrix-size m))))
