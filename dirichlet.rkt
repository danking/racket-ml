#lang typed/racket

(require math
         "more-math.rkt"
         )

(provide dirichlet-dist
         )

(define-type RealVector [Vectorof Real])

(: dirichlet-dist : (RealVector
                     ->
                     (distribution RealVector RealVector)))
(define (dirichlet-dist alphas)
  (: dirichlet-pdf : (case-> (RealVector -> Flonum)
                             (RealVector (U Any False) -> Flonum)))
  (define (dirichlet-pdf xs [log? #f])
    (if (for/or: : Boolean
                 ([x : Real (in-vector xs)])
          (or (< x 0) (> x 1)))
        (error 'dirichlet-pdf
               (string-append "All elements of the vector must be between 0 "
                              "and 1, inclusive, given ~a.")
               xs)
        (if log?
            (- (for/fold: : Flonum
                          ([result : Flonum #i0])
                          ([alpha alphas]
                           [x xs])
                 (+ result (fl (* (sub1 alpha) (log (max 0 x)))))))
            (/ (for/fold: : Flonum
                          ([result : Flonum #i1])
                          ([alpha alphas]
                           [x xs])
                 (* result (fl (expt (max 0 x) (sub1 alpha)))))
               (multivariate-beta alphas)))))

  (: dirichlet-sampler : (case-> (-> RealVector)
                                 (Integer -> [Listof RealVector])))
  (define (dirichlet-sampler [n #f])
    (if n
        (for/list: : [Listof RealVector]
                   ([i (in-range 0 n)])
          (dirichlet-sample-one))
        (dirichlet-sample-one)))

  (: dirichlet-sample-one : (-> RealVector))
  (define (dirichlet-sample-one)
    (let* ((gammas (for/vector: : RealVector
                                ([alpha alphas])
                     (sample (gamma-dist alpha 1))))
           (sum (for/fold: : Flonum ([sum #i0]) ([gamma gammas])
                  (+ sum gamma))))
      (for/vector: : RealVector
                   ([gamma gammas])
        (/ gamma sum))))


  (distribution dirichlet-pdf dirichlet-sampler))
