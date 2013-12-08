#lang typed/racket

(require math
         )
(provide multivariate-beta
         )

(: multivariate-beta : ([Vectorof Real] [#:log? Boolean] -> Real))
(define (multivariate-beta vec #:log? [log? #f])
  (if log?
      (- (for/sum: : Real ([v vec]) (log-gamma v))
         (log-gamma (for/sum: : Real ([v vec]) v)))
      (/ (for/product: : Real ([v vec]) (gamma v))
         (gamma (for/sum: : Real ([v vec]) v)))))
