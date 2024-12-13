#lang racket

(module+ test
  (require rackunit))

(define COLON-DELIMIT ": ")
(define SPACE-DELIMIT " ")

(define (part-a)
  (define cal-res (cal-res/ip))
  (println cal-res))

;; Asssume operands are non empty.

;; Given how many unique operators to make, 
;; Outputs a stream of sequence of operators.
(define (in-unique-operators num operators)
  (sequence-map
   (λ (combinations-group)
     (in-vector
      (for/fold ([outer-vec (make-vector num #f)])
                ([comb-g combinations-group]
                 [curr-op (in-list operators)])
        (for/fold ([inner-vec outer-vec])
                  ([i comb-g])
          (vector-set/copy inner-vec i curr-op)))))
   (in-combinations-indices num (length operators))))

;; Nat Nat -> [Sequenceof [List [Listof Nat]]]
;; Outputs a sequence, where each element is a list of lists of
;; indices, when the length of the outer list is the given number of
;; groups, and the inner lists are the cumulative groups. The indices
;; in range [0,list-len) are distributed among the groups. Each
;; grouping is a unique distribution of the indices. The ith group
;; contains elements in the (i+1)th group.
(define (in-combinations-indices list-len num-groups)
  (define (get-all-indices len) (build-list len (λ (i) i)))

  ; [Listof Nat] (Combination) Nat -> [Sequenceof [Listof [Listof Nat]]]
  ; Given the combination and number of groups remaining, gets all
  ; unique distributions for the number of remaining groups.
  (define (help/remaining curr-combination rem-num-groups)
    (cond
      [(= rem-num-groups 1)
       (sequence-map
        (λ (curr-group) (list curr-group))
        (in-combinations curr-combination))]
      [(> rem-num-groups 1)
       (sequence-fold
        (λ (acc curr-group)
          (sequence-append
           acc
           (sequence-map
            (λ (rem-groups) (cons curr-group rem-groups))
            (help/remaining curr-group (sub1 rem-num-groups)))))
        empty-sequence
        (in-combinations curr-combination))]))

  (define all-indices (get-all-indices list-len))
  (cond
    [(= num-groups 1)
     (in-list (list (list all-indices)))]
    [(> num-groups 1)
     (sequence-map
      (λ (rem-groups) (cons all-indices rem-groups))
      (help/remaining all-indices (sub1 num-groups)))]))
;; 0 1 2 3 4
;; 1 2 3 -> 2 3 -> 3
;; 1 2 3 -> 2   -> X

;; Gets a stream of combination lists (list of list of indices), of
;; the given number of groups, where all indices of a list of the
;; given length are in the total grouping, and no two indices in the
;; same grouping appears twice.
#;(define (in-combinations-indices list-len num-groups)

  ; For each way we can distribute the length of each group,
  ; Find all combinations from left to right, flatten as one sequence.

  (sequence-fold
   (λ (acc num-groupings)
     (sequence-append
      acc
      (in-combinations )))
   empty-sequence
   (in-distribute-num list-len num-groups))

  
  (sequence-fold
   (λ (acc curr) (sequence-append acc curr))
   empty-sequence
   (sequence-map
   (λ (lon)
     (sequence-map
      (λ (num-distributions)
        (partition-lon lon num-distributions))
      (in-distribute-num list-len num-groups)))
   (in-permutations (build-list list-len (λ (i) i))))))

;; E.g '(1 2 3 4) '(1 2 1) -> '((1) (2 3) (4))
;; Assumes that the the sum of num-distributions equal to length of
;; lon.
(define (partition-lon lon0 num-distributions)
  (for/fold ([lon lon0]
             [rv-out '()]
             #:result (reverse rv-out))
            ([to-take num-distributions])
    (define-values (taken left) (split-at lon to-take))
    (values left (cons taken rv-out))))

;; num-groups-remaining includes the current group.
;; num-remaining includes gnum.
(define (in-distribute-num num0 num-groups)
  (define (help/curr-group gnum num-remaining
                           num-groups-remaining)
    (cond
      [(= num-groups-remaining 1)
       (in-list (list (list num-remaining)))]
      [(= gnum 0)
       (sequence-map
        (λ (lon) (cons gnum lon))
        (in-distribute-num num-remaining
                           (sub1 num-groups-remaining)))]
      [else
       (sequence-append
        (sequence-map
         (λ (lon) (cons gnum lon))
         (in-distribute-num (- num-remaining gnum)
                            (sub1 num-groups)))
        (help/curr-group (sub1 gnum) num-remaining
                         num-groups-remaining))]))
  (help/curr-group num0 num0 num-groups))

(define (cal-res/ip [operators (list + *)] [ip (current-input-port)])
  (for/sum ([l (in-lines ip)])
    (define numstr+numsstr (string-split l COLON-DELIMIT))
    (define total-num (string->number (first numstr+numsstr)))
    (define nums (map string->number (string-split (second numstr+numsstr) " ")))
    (if (can-make-equation? total-num nums operators) total-num 0)))
(define (can-make-equation? total operands operators)
  (for/first ([unique-operators-seq
               (in-unique-operators (sub1 (length operands)) operators)]
              #:when (can-make-equation?/ops total operands
                                             unique-operators-seq))
    #t))
(define (can-make-equation?/ops total operands ops-seq)
  (= total
     (for/fold ([acc (first operands)])
               ([op ops-seq]
                [operand (in-list (rest operands))])
       #:break (> acc total)
       (op acc operand))))

(define (part-b)
  (define cal-res (cal-res/ip (list + * num-concat)))
  (println cal-res))

(define (num-concat a b)
  (+ (* (expt 10 (floor (add1 (log b 10)))) a)
     b)
  
  #;(string->number (string-append (number->string a)
                                   (number->string b))))

(time
 (with-input-from-file "./Tests/puzzle.txt"
   (λ ()
     (part-b))))

(command-line
 #:program "1"
 #:once-each
 [("-a" "--a") "Run part a entry point"
               (part-a)]
 [("-b" "--b") "Run part a entry point"
               (part-b)])
