#lang racket

(module+ examples
  (provide ls-mt
           ls-7/6
           ls-7/6/4/2/1
           ls-1/2/7/8/9
           ls-9/7/6/2/1
           ls-1/3/2/4/5
           ls-8/6/4/4/1
           ls-1/3/6/7/9

           r-7/6/4/2/1~1/2/7/8/9))

(module+ test
  (require rackunit

           (submod ".." examples)))

(define MIN-DIFF 1)
(define MAX-DIFF 3)

(define LEVEL-SEPARATOR " ")



;; A Level is a Nat.

;; A Report is a [Listof [Listof Level]]

(module+ examples
  (define ls-mt '())
  (define ls-7/6 '(7 6))
  (define ls-7/6/4/2/1 '(7 6 4 2 1))
  (define ls-1/2/7/8/9 '(1 2 7 8 9))
  (define ls-9/7/6/2/1 '(9 7 6 2 1))
  (define ls-1/3/2/4/5 '(1 3 2 4 5))
  (define ls-8/6/4/4/1 '(8 6 4 4 1))
  (define ls-1/3/6/7/9 '(1 3 6 7 9))

  (define r-7/6/4/2/1~1/2/7/8/9
    (list ls-7/6/4/2/1 ls-1/2/7/8/9)))

(define (part-a)
  (define report (parse))
  (define num-safe (num-safe/report report))
  (println num-safe))

(define (parse [ip (current-input-port)])
  (for/fold ([rv-report '()] #:result (reverse rv-report))
            ([line (in-lines ip)])
    (cons (map string->number (string-split line LEVEL-SEPARATOR))
          rv-report)))

(module+ test
  (define-simple-check (check-parse input-str expected)
    (define ip (open-input-string input-str))
    (check-equal? (parse ip) expected))

  (check-parse "" (list))
  (check-parse "\n" (list ls-mt))
  (check-parse "7 6" (list ls-7/6))
  (check-parse "7 6\n" (list ls-7/6))
  (check-parse "1 2 7 8 9\n" (list ls-1/2/7/8/9))
  (check-parse "1 2 7 8 9\n9 7 6 2 1"
               (list ls-1/2/7/8/9 ls-9/7/6/2/1)))

;; Report -> Nat
(define (num-safe/report report [s?/l/c safe?/levels/comp])
  (for/sum ([levels (in-list report)])
    (if (safe?/levels levels s?/l/c) 1 0)))

(module+ test
  (check-equal? (num-safe/report r-7/6/4/2/1~1/2/7/8/9) 1))

;; [NEListof Level] -> Boolean
(define (safe?/levels levels [s?/l/c safe?/levels/comp])
  (or (s?/l/c levels <)
      (s?/l/c levels >)))
(define (safe?/levels/comp levels comp)
  (for/fold ([prev (first levels)]
             [safe? #t]
             #:result safe?)
            ([curr (rest levels)])
    #:break (not safe?)
    (define diff (- curr prev))
    (if (and (comp diff 0) (<= MIN-DIFF (abs diff) MAX-DIFF))
        (values curr #t)
        (values curr #f))))
(module+ test
  (check-true (safe?/levels ls-7/6))
  (check-true (safe?/levels ls-7/6/4/2/1))
  (check-false (safe?/levels ls-1/2/7/8/9))
  (check-false (safe?/levels ls-9/7/6/2/1))
  (check-false (safe?/levels ls-1/3/2/4/5))
  (check-false (safe?/levels ls-8/6/4/4/1))
  (check-true (safe?/levels ls-1/3/6/7/9)))

(define (part-b)
  (define report (parse))
  (define num-safe (num-safe/report report
                                    safe?/levels/comp/dampener))
  (println num-safe))

;; [NEListof Level] [Nat Nat -> Boolean] -> Boolean
(define (safe?/levels/comp/dampener levels comp)
  (or (safe?/levels/comp levels comp)
      (for/first ([l (in-remove-one levels)]
                  #:when (safe?/levels/comp l comp))
        #t)))
;; {X} [NEListof X] -> [Streamof [NEListof X]]
;; Creates a stream of all possible lists of the given list with one
;; element removed.
(define (in-remove-one ls [prevs '()])
  (cond
    [(empty? ls) empty-stream]
    [(cons? ls)
     (stream-cons (append (reverse prevs) (rest ls))
                  (in-remove-one (rest ls)
                                 (cons (first ls) prevs)))]))
(module+ test
  (check-equal? (stream->list (in-remove-one '(1 2 3)))
                '((2 3) (1 3) (1 2))))

(module+ test
  (check-true (safe?/levels ls-7/6 safe?/levels/comp/dampener))
  (check-true (safe?/levels ls-7/6/4/2/1 safe?/levels/comp/dampener))
  (check-false (safe?/levels ls-1/2/7/8/9 safe?/levels/comp/dampener))
  (check-false (safe?/levels ls-9/7/6/2/1 safe?/levels/comp/dampener))
  (check-true (safe?/levels ls-1/3/2/4/5 safe?/levels/comp/dampener))
  (check-true (safe?/levels ls-8/6/4/4/1 safe?/levels/comp/dampener))
  (check-true (safe?/levels ls-1/3/6/7/9 safe?/levels/comp/dampener)))

(command-line
 #:program "1"
 #:once-each
 [("-a" "--a") "Run part a entry point"
               (part-a)]
 [("-b" "--b") "Run part a entry point"
               (part-b)])
