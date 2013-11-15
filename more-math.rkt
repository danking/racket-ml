#lang typed/racket

(require math
         )
(provide multi-beta
         )

(: multi-beta : [Vectorof Real] -> Real)
(define (multi-beta vec)
  (/ (for/product: : Real ([v vec]) (gamma v))
     (gamma (for/sum: : Real ([v vec]) v))))
