#lang typed/racket

(require plot/typed
         (only-in plot/typed/utils linear-seq)
         (only-in racket/snip image-snip%)
         )

(require/typed racket
               [in-value (All (X) (X -> [Sequenceof X]))]
               )

(provide (struct-out histogram)
         histogram->function
         hist-gen&render
         histogram->renderer
         generate-histogram
         )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Histogram

(struct: histogram
         ([bins : [Vectorof Real]]
          [left : Real]
          [right : Real]
          [number-of-bins : Natural]
          [bin-width : Real]
          [which-bin : (Real -> Natural)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: histogram->function : (histogram -> (Real -> Real)))
(define/match (histogram->function h)
  [((histogram bins left right _ _ which-bin))
   (lambda: ([n : Real])
     (when (or (< n left) (> n right))
       (error 'histogram-as-function
              "The value ~a is not within the range of this histogram [~a,~a]"
              n left right))
     ((inst vector-ref Real) bins (which-bin n)))])

(: bucketize : ((Sequenceof Real)
                (Sequenceof Real)
                Natural
                (Real -> Natural)
                Real
                [#:normalize? Boolean]
                ->
                (Vectorof Real)))
(define (bucketize data
                   weights
                   number-of-bins
                   which-bin
                   bin-width
                   #:normalize? [normalize? #f])
  (define bins
    (for/fold:
        ([hist : [Vectorof Real] (make-vector number-of-bins 0)])
        ([d : Real data]
         [n : Real weights])
        (let ((bin (which-bin d)))
          (vector-set! hist bin (+ (vector-ref hist bin) n))
          hist)))

  (if normalize? (normalize-vector bins bin-width) bins))

(: generate-which-bin : (Real Real Natural Real -> (Real -> Natural)))
;; generate a function which partitions the line segment [left, right] into
;; number-of-bins (almost) equally sized bins.
;;
;; NB: The last bin is inclusive of both endpoints whereas all other bins are
;; exclusive of the right-most endpoint.
(define (generate-which-bin left right number-of-bins bin-width)
  (lambda: ([value : Real])
    (if (and (>= value left) (<= value right))
        (max (exact-floor (/ (- value left) bin-width))
             0)                         ; appease the type checker
        (error 'histogram-which-bin
               "Value ~a is not within range [~a,~a]."
               value left right))))

(: normalize-vector ((Vectorof Real) Real -> (Vectorof Real)))
(define (normalize-vector v bin-width)
  (define sum (for/sum: : Real ([value : Real (in-vector v)])
                (* bin-width value)))

  (for/vector: : (Vectorof Real) ([value : Real (in-vector v)])
    (/ value sum)))

(: sequence-of-ones : [Sequenceof Natural])
(define sequence-of-ones (in-value 1))

(: generate-histogram : ([Sequenceof Real]
                         Natural
                         [#:weights [Sequenceof Natural]]
                         [#:normalize? Boolean]
                         ->
                         histogram))
(define (generate-histogram data
                            number-of-bins
                            #:weights [weights sequence-of-ones]
                            #:normalize? [normalize? #t])
  (define left (exact-floor (apply min (sequence->list data))))
  (define right (exact-ceiling (apply max (sequence->list data))))
  (define total-width (max 0 (- right left)))
  (define bin-width (max 0 (/ total-width number-of-bins)))
  (define which-bin
    (generate-which-bin left right number-of-bins bin-width))

  (histogram (bucketize data
                        weights
                        number-of-bins
                        which-bin
                        bin-width
                        #:normalize? normalize?)
             left right
             number-of-bins
             bin-width
             which-bin))

(: histogram->renderer : (histogram -> renderer2d))
(define/match (histogram->renderer h)
  [((histogram _ left right number-of-bins _ _))
   (area-histogram (histogram->function h) (linear-seq left right number-of-bins))])

;; poorly named, but skips the intermediate step for the impatient among us
(: hist-gen&render : ([Sequenceof Real]
                      Natural
                      [#:weights [Sequenceof Natural]]
                      [#:normalize? Boolean]
                      ->
                      renderer2d))
(define (hist-gen&render data
                         number-of-bins
                         #:weights [weights sequence-of-ones]
                         #:normalize? [normalize? #t])
  (histogram->renderer (generate-histogram data
                                           number-of-bins
                                           #:weights weights
                                           #:normalize? normalize?)))
