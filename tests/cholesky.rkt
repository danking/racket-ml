#lang racket

(require math
         rackunit
         "../cholesky.rkt"
         )

(define/provide-test-suite cholesky-tests
  (check-equal? (cholesky (matrix [[25 15 -5]
                                   [15 18  0]
                                   [-5  0 11]]))
                (matrix [[5 0 0]
                         [3 3 0]
                         [-1 1 3]]))

  (check-equal?
   (cholesky (matrix [[18 22  54 42]
                      [22 70  86 62]
                      [54 86 174 134]
                      [42 62 134 106]]))
   (matrix [[4.242640687119285 0 0 0]
            [ 5.185449728701349 6.565905201197403  0                  0]
            [12.727922061357857 3.0460384954008553 1.6497422479090704 0]
            [ 9.899494936611665 1.6245538642137891 1.849711005231382  1.3926212476455924]]))

  (check-equal? (cholesky (matrix [[4 12 -16]
                                   [12 37 -43]
                                   [-16 -43 98]]))
                (matrix [[2 6 -8]
                         [0 1 5]
                         [0 0 3]])))
