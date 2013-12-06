#lang typed/racket

(require "cholesky.rkt"
         "dirichlet.rkt"
         "histogram.rkt"
         "more-math.rkt"
         )

(provide ;; cholesky
         cholesky
         ;; dirichlet
         dirichlet-dist
         ;; histogram
         (struct-out histogram)
         histogram->function
         hist-gen&render
         histogram->renderer
         generate-histogram
         ;; more-math
         multivariate-beta
         )
