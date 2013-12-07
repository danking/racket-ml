#lang typed/racket

(require math
         )
(provide multivariate-beta
         )

(: multivariate-beta : [Vectorof Real] -> Real)
(define (multivariate-beta vec)
  (/ (for/product: : Real ([v vec]) (gamma v))
     (gamma (for/sum: : Real ([v vec]) v))))
