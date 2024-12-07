#lang racket

(module+ examples
  (provide bm-mt bm-2-bf-same-bf-1 bm-2-bf-1
           bm-97-cannot-bf-61
           bm-61-cannot-bf-97
           bm-75 bm-47 bm-61 bm-53
           bm-75/47/61/53

           up-mt up-75 up-97-61 up-97-61-53
           up-75-47-61-53-29
           ups-75/97-61-53))

(module+ test
  (require rackunit
           (submod ".." examples)))

;; A SafetyManual is a (safety-manual BanMapping [Sequenceof Update])
;; Represents a mapping from a page to a set of pages that cannot be
;; seen after that page, as well as a stream of list of updates.
(struct safety-manual [ban-map updates-stream])

;; A BanMapping is a [Hash Page [Setof Page]]

;; A Page is a Nat

;; A Update is a [Listof Page]

(define RX-X\Y #rx"([0-9]+)\\|([0-9]+)")

(module+ examples
  (define bm-mt (hash))
  (define bm-2-bf-same-bf-1
    (hash 29 (set 97 47)
          13 (set 75)
          11 (set 29)))
  (define bm-2-bf-1
    (hash 29 (set 11)
          97 (set 29)
          47 (set 29)
          75 (set 13)))
  (define bm-97-cannot-bf-61
    (hash 61 (set 97)))
  (define bm-61-cannot-bf-97
    (hash 97 (set 61)))
  (define bm-75 (hash 75 (set 47 61 53 29)))
  (define bm-47 (hash 47 (set 61 53 29)))
  (define bm-61 (hash 75 (set 61)
                      47 (set 61)
                      61 (set 53 29)))
  (define bm-53 (hash 53 (set 29)))
  (define bm-75/47/61/53
    (hash 75 (set 47 61 53 29)
          47 (set 61 53 29)
          61 (set 53 29)
          53 (set 29)))

  (define up-mt '())
  (define up-75 '(75))
  (define up-97-61 '(97 61))
  (define up-97-61-53 '(97 61 53))
  (define up-75-47-61-53-29 '(75 47 61 53 29))
  (define ups-75/97-61-53 (in-list (list up-75 up-97-61-53))))

(define (part-a)
  (define sm (parse))
  (define sum (sum-mid-corrects/sm sm))
  (println sum))

(define (parse [ip (current-input-port)])
  (define in-lines-ip (in-lines ip))
  (safety-manual (get-bm in-lines-ip ip) (in-updates in-lines-ip)))
;; [Sequenceof Line] -> BannedMapping
;; Terminates when no more lines or empty line found.
(define (get-bm in-lines-ip ip)
  (for/fold ([bm (hash)])
            ([line in-lines-ip])
    #:final (equal? (peek-char ip) #\newline)
    (match-define (list x cannot-come-bf-x)
      (map string->number (drop (regexp-match RX-X\Y line) 1)))
    (hash-update bm x
                 (λ (old-set)
                   (set-add old-set
                            cannot-come-bf-x))
                 (set))))
(define (in-updates lines-seq)
  (sequence-map (λ (l) (map string->number (string-split l ",")))
                (sequence-filter non-empty-string? lines-seq)))
(module+ test
  (define-simple-check (check-parse-sm input-str exp-ban-map exp-updates)
    (define ip (open-input-string input-str))
    (define act-sm (parse ip))
    (check-equal? (safety-manual-ban-map act-sm) exp-ban-map)
    (check-equal?
     (sequence->list (safety-manual-updates-stream act-sm))
     exp-updates))

  (check-parse-sm "1|2\n\n75" (hash 1 (set 2)) '((75)))
  (check-parse-sm "97|29\n47|29\n75|13\n29|11\n\n75\n97,61,53"
                  bm-2-bf-1
                  (sequence->list ups-75/97-61-53))
  (check-parse-sm "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13"
                  (hash 29 (set 13)
                        47 (set 53 13 61 29)
                        53 (set 29 13)
                        61 (set 13 53 29)
                        75 (set 29 53 47 61 13)
                        97 (set 13 61 47 29 53 75))
                  '()))

;; SafetyManual -> Nat
(define (sum-mid-corrects/sm sm)
  (for/sum ([update (safety-manual-updates-stream sm)])
    (if (legal/update? update (safety-manual-ban-map sm))
        (get-mid-element update)
        0)))
(module+ test
  (check-equal? (sum-mid-corrects/sm
                 (safety-manual bm-mt ups-75/97-61-53))
                (+ 75 61))
  (check-equal? (sum-mid-corrects/sm
                 (safety-manual bm-2-bf-same-bf-1
                                ups-75/97-61-53))
                (+ 75 61))
  (check-equal? (sum-mid-corrects/sm
                 (safety-manual bm-97-cannot-bf-61
                                ups-75/97-61-53))
                (+ 75)))

;; Update BanMapping -> Boolean
(define (legal/update? update bm)
  (for/fold ([seen-pages (set)]
             [okay? #t]
             #:result okay?)
            ([page (in-list update)])
    #:break (not okay?)
    (values (set-add seen-pages page)
            (not (contains-illegal?/bm bm page seen-pages)))))
(module+ test
  (check-true (legal/update? up-mt bm-2-bf-same-bf-1))
  (check-true (legal/update? up-97-61-53 bm-mt))
  (check-false (legal/update? up-97-61-53 bm-97-cannot-bf-61))
  (check-true (legal/update? up-75-47-61-53-29 bm-75/47/61/53))
  (check-true (legal/update? up-97-61-53 bm-61-cannot-bf-97)))

;; BanMapping Page [Setof Page] -> Boolean
(define (contains-illegal?/bm bm page seen-pages)
  (define banned-pages (hash-ref bm page (set)))
  (not (set-empty? (set-intersect banned-pages seen-pages))))
(module+ test
  (check-false (contains-illegal?/bm bm-97-cannot-bf-61 97 (set 61)))
  (check-true (contains-illegal?/bm bm-97-cannot-bf-61 61 (set 97)))

  (check-false (contains-illegal?/bm bm-2-bf-same-bf-1 11 (set 47)))
  (check-false (contains-illegal?/bm bm-75 75 (set)))
  (check-false (contains-illegal?/bm bm-47 47 (set 75)))
  (check-false (contains-illegal?/bm bm-61 61 (set 75 47)))
  (check-false (contains-illegal?/bm bm-53 53 (set 75 47 61))))

;; {X} [Listof X] -> X
;; Gets the middle element of the given list, assuming that the list
;; has an odd length.
(define (get-mid-element lox)
  (list-ref lox (/ (sub1 (length lox)) 2)))
(module+ test
  (check-equal? (get-mid-element '(1)) 1)
  (check-equal? (get-mid-element '(a b c)) 'b)
  (check-equal? (get-mid-element '(x y 2 b 6)) 2))

(define (part-b)
  (define sm (parse))
  (define sum (sum-mid-updated/sm sm))
  (println sum))

;; SafetyManual -> Nat
(define (sum-mid-updated/sm sm)
  (for/sum ([update (safety-manual-updates-stream sm)])
    (if (legal/update? update (safety-manual-ban-map sm))
        0
        (get-mid-element
         (reorder-update update (safety-manual-ban-map sm))))))

;; Update BannedMapping -> Update
;; Reorders the given update such that it becomes legal given the
;; banned mapping.
(define (reorder-update update bm)
  (define reduced-bm (reduce/bm bm update))
  (define page->num-pages-after (get-page->num-pages-after reduced-bm))
  (order-page->num-pages-after page->num-pages-after))

;; Reduces the given ban mapping such that only pages that are in the
;; given update are kept.
(define (reduce/bm bm update)
  (for/fold ([acc (hash)])
            ([(k v) (in-hash bm)])
    (if (member k update)
        (hash-set acc k
                  (list->set (filter (λ (p) (member p update))
                                     (set->list v))))
        acc)))

;; Assumes that all pages in the given ban mapping are in the
;; update. Maps each page to the number of pages that has to appear
;; after.
(define (get-page->num-pages-after reduced-bm)
  (for/fold ([memo (hash)])
            ([(p bp) (in-hash reduced-bm)])
    (match-define (list _ new-memo)
      (get-page->num-pages-after/p p memo reduced-bm))
    new-memo))

(define (get-page->num-pages-after/bp bp memo0 reduced-bm)
  (for/foldr ([memo memo0]
              [nums '()]
              #:result (list nums memo))
             ([p (in-list bp)])
    (match-define (list num new-memo)
      (get-page->num-pages-after/p p memo reduced-bm))
    (values new-memo (cons num nums))))

(define (get-page->num-pages-after/p p memo reduced-bm)
  (cond
    [(hash-ref memo p #f)
     =>
     (λ (n) (list n memo))]
    [else
     (define bp (set->list (hash-ref reduced-bm p (set))))
     (match-define (list nums new-memo)
       (get-page->num-pages-after/bp bp memo reduced-bm))
     (define num (add1 (apply max -1 nums)))
     (list num (hash-set new-memo p num))]))

(define (order-page->num-pages-after page->num-pages-after)
  (map car (sort (hash->list page->num-pages-after)
                 #:key cdr
                 >)))

(module+ test
  (check-equal? (reorder-update up-97-61 bm-97-cannot-bf-61)
                '(61 97))

  (define (check-reorder update bm-list new-update)
    (define bm
      (for/hash ([p-bp-pair bm-list])
        (match-define (cons page banned-pages) p-bp-pair)
        (values page (list->set banned-pages))))
    
    (check-equal? (reorder-update update bm)
                  new-update))

  (define-simple-check (check-reorder/io in out bml-list)
    (for ([bm-list bml-list])
      (check-reorder in bm-list out)))

  (check-reorder/io '(1 2 3) '(2 1 3)
                    '(((2 . (1))
                       (1 . (3)))
                      ((2 . (1 3))
                       (1 . (3)))))
  (check-reorder/io '(1 2 3) '(3 1 2)
                    '(((3 . (1))
                       (1 . (2)))
                      ((3 . (1 2))
                       (1 . (2)))))
  (check-reorder/io '(1 2 3) '(1 3 2)
                    '(((1 . (3))
                       (3 . (2)))
                      ((1 . (3 2))
                       (3 . (2)))))
  )

(command-line
 #:program "1"
 #:once-each
 [("-a" "--a") "Run part a entry point"
               (part-a)]
 [("-b" "--b") "Run part a entry point"
               (part-b)])
