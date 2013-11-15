#lang typed/racket

(require math
         "more-math.rkt"
         )

(provide dirichlet-dist
         )

(define-type RealVector [Vectorof Real])

(: dirichlet-dist : (RealVector -> (distribution RealVector RealVector)))
(define (dirichlet-dist alphas)
  (: dirichlet-pdf : (case-> (RealVector -> Flonum)
                             (RealVector (U Any False) -> Flonum)))
  (define (dirichlet-pdf xs [log? #f])
    ;; TODO Actually have smart log behavior
    (if log?
        (error 'dirichlet-pdf "log behavior unimplemented")
        (/ (for/fold: : Flonum
                      ([result : Flonum #i1])
                      ([alpha alphas]
                       [x xs])
             (* result (fl (expt (min 1 (max 0 x)) (sub1 alpha)))))
           (multi-beta alphas))))

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
