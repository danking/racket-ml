#lang racket

(require rackunit
         rackunit/text-ui
         math
         "../multivariate-normal.rkt"
         )

;; I'm not really sure how to check distributions. I pulled these values from
;; the mvtnorm R package, so I lightly trust them. I should at least be as good
;; as R, right?

(define zero-mean2 (matrix [[0] [0]]))
(define identity-covariance2 (identity-matrix 2))

(define zero-mean3 (matrix [[0] [0] [0]]))
(define identity-covariance3 (identity-matrix 3))

(define 10-5-0-mean (matrix [[10] [5] [0]]))
(define all-5s-covariance3 (matrix [[5 5 5]
                                    [5 5 5]
                                    [5 5 5]]))

(define epsilon 1e-7)

(define-test-suite mvn-tests
  (test-case
   "2d tests"
   (check-= (pdf (multivariate-normal-dist zero-mean2 identity-covariance2)
                 (matrix [[0] [0]]))
            0.1591549
            epsilon)

   (for ((pair   '((10 5)       (5 0)        (1.5 3.5)    (0.1 0.8)))
         (result '(1.143971e-28 5.931153e-07 0.0001130278 0.1149938)))
     ;; because we're using identity covariance and zero mean the reversed
     ;; coordinates should have the same value
     ;; additionally the sign of the coordinates shouldn't matter
     (for ((x pair)          ;; e.g. (10 5)
           (y (reverse pair)) ;; e.g. (5 10)
           ;; thus (x,y) is the sequence (10,5) (5,10)
           (signs `((,- ,-) (,- ,+) (,+ ,-) (,+ ,+))))
       (check-= (pdf (multivariate-normal-dist zero-mean2 identity-covariance2)
                     (matrix [[((first signs) x)]
                              [((second signs) y)]]))
                result
                epsilon))))

  (test-case
   "3d tests"
   (check-= (pdf (multivariate-normal-dist zero-mean3 identity-covariance3)
                 (matrix [[0] [0] [0]]))
            0.06349364
            epsilon)
   (check-= (pdf (multivariate-normal-dist 10-5-0-mean identity-covariance3)
                 (matrix [[0] [0] [0]]))
            4.563784e-29
            epsilon)
   (check-= (pdf (multivariate-normal-dist 10-5-0-mean identity-covariance3)
                 (matrix [[1] [1] [1]]))
            3.328899e-23
            epsilon)
   (check-= (pdf (multivariate-normal-dist 10-5-0-mean identity-covariance3)
                 (matrix [[10] [10] [10]]))
            4.563784e-29
            epsilon)
   (check-= (pdf (multivariate-normal-dist 10-5-0-mean identity-covariance3)
                 (matrix [[10] [-10] [10]]))
            4.563784e-29
            epsilon)))

(run-tests mvn-tests)
