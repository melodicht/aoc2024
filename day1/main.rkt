#lang racket

(module+ examples
  (provide lp-mt
           lp-3/4 lp-3/4-4/3

           wsa-mt
           wsa-3/4 wsa-3/4-4/3

           wsb-mt
           wsb-3/4 wsb-3/4-4/3))

(module+ test
  (require (submod ".." examples)))


;; A ListPair is a [Pairof [Listof Nat] [Listsof Nat]]

(module+ examples
  (define lp-mt '(() . ()))
  (define lp-3/4 '((3) . (4)))
  (define lp-3/4-4/3 '((3 4) . (4 3))))

;; A WorldStateA is a ListPair
;; where each of the list is sorted in ascending order.

(module+ examples
  (define wsa-mt '(() . ()))
  (define wsa-3/4 '((3) . (4)))
  (define wsa-3/4-4/3 '((3 4) . (3 4))))

(module+ test
  (require rackunit))

(define (part-a)
  (define ls-pair (parse))

  ;; Mapping each element to a pair of it's original index and its
  ;; value, then sort each list by its values.
  (define wsa (ls-pair->wsa ls-pair))

  ;; Loop through the two lists pairwise, and sum up the difference in
  ;; index.
  (define total-dist (total-dist/wsa wsa))
  
  (println total-dist))

(define (ls-pair->wsa lsp)
  (match-define (cons ll rl) lsp)
  (cons (sort ll <) (sort rl <)))

(module+ test
  (check-equal? (ls-pair->wsa lp-mt) wsa-mt)
  (check-equal? (ls-pair->wsa lp-3/4) wsa-3/4)
  (check-equal? (ls-pair->wsa lp-3/4-4/3) wsa-3/4-4/3))

(define (total-dist/wsa ws)
  (match-define (cons ll rl) ws)
  (for/sum ([l (in-list ll)]
            [r (in-list rl)])
    (abs (- l r))))

(module+ test
  (check-equal? (total-dist/wsa wsa-mt) 0)
  (check-equal? (total-dist/wsa wsa-3/4) 1)
  (check-equal? (total-dist/wsa wsa-3/4-4/3) 0))

(define INLINE-SEPARATOR "   ")

(define (parse [ip (current-input-port)])
  (for/fold ([rv-left-list '()]
             [rv-right-list '()]
             #:result (cons (reverse rv-left-list)
                            (reverse rv-right-list)))
            ([line (in-lines ip)])
    (match-define (list l r)
      (map string->number (string-split line INLINE-SEPARATOR)))
    (values (cons l rv-left-list) (cons r rv-right-list))))

(module+ test
  (define-simple-check (check-parse input-str expected)
    (define ip (open-input-string input-str))
    (define actual (parse ip))
    (check-equal? actual expected))

  (check-parse "" lp-mt)
  (check-parse "3   4" lp-3/4)
  (check-parse "3   4\n" lp-3/4)
  (check-parse "3   4\n4   3\n" lp-3/4-4/3))


;; A WorldStateB is a [Pairof [Listof Nat] [Hash Nat Nat]]
;; Where the left of pair is the left list, and the right of the pair
;; is a mapping from an element in the right list to its frequency in
;; the right list.

(module+ examples
  (define wsb-mt `(() . ,(hash)))
  (define wsb-3/4 `((3) . ,(hash 4 1)))
  (define wsb-3/4-4/3 `((3 4) . ,(hash 4 1 3 1))))

(define (part-b)
  (define ls-pair (parse))
  (define wsb (ls-pair->wsb ls-pair))
  (define similarity-score (similarity-score/wsb wsb))
  (println similarity-score))

;; ListPair -> WorldStateB
(define (ls-pair->wsb lsp)
  (match-define (cons ll rl) lsp)
  (cons ll (get-frequency rl)))
(module+ test
  (check-equal? (ls-pair->wsb lp-mt) wsb-mt)
  (check-equal? (ls-pair->wsb lp-3/4) wsb-3/4)
  (check-equal? (ls-pair->wsb lp-3/4-4/3) wsb-3/4-4/3))

;; {X} [Listof X] -> [Hash X Nat]
(define (get-frequency l)
  (for/fold ([counter (hash)])
            ([el (in-list l)])
    (hash-update counter el add1 0)))
(module+ test
  (check-equal? (get-frequency '()) (hash))
  (check-equal? (get-frequency '(1 2)) (hash 1 1 2 1))
  (check-equal? (get-frequency '(a a a a)) (hash 'a 4)))

;; WorldStateB -> Nat
(define (similarity-score/wsb wsb)
  (match-define (cons ll rl-counter) wsb)
  (for/sum ([l (in-list ll)])
    (* l (hash-ref rl-counter l 0))))
(module+ test
  (check-equal? (similarity-score/wsb wsb-mt) 0)
  (check-equal? (similarity-score/wsb wsb-3/4) 0)
  (check-equal? (similarity-score/wsb wsb-3/4-4/3) 7))

(command-line
 #:program "1"
 #:once-each
 [("-a" "--a") "Run part a entry point"
               (part-a)]
 [("-b" "--b") "Run part a entry point"
               (part-b)])


