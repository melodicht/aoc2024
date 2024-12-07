#lang racket

(module+ examples
  (provide mps-mt
           mps-2*4
           mps-2*4/5*5
           mps-2*4/5*5/11*8
           mps-2*4/5*5/11*8/8*5))

(module+ test
  (require rackunit

           (submod ".." examples)))

(define MUL-REGEX #px"mul\\(([0-9]{1,3}),([0-9]{1,3})\\)")
(define DO-BSTR #"do()")
(define DONT-BSTR #"don't()")
(define MUL-DO/NT-REGEX
  #px"(?:mul\\(([0-9]{1,3}),([0-9]{1,3})\\)|do\\(\\)|don't\\(\\))")

;; A MulPair is a [Pair Nat Nat]
;; where Nat is three digits.

(define mps-mt '())
(define mps-2*4 '((2 . 4)))
(define mps-2*4/5*5 '((2 . 4) (5 . 5)))
(define mps-2*4/5*5/11*8 '((2 . 4) (5 . 5) (11 . 8)))
(define mps-2*4/5*5/11*8/8*5 '((2 . 4) (5 . 5) (11 . 8) (8 . 5)))

(define (part-a)
  (define sum (get-sum/ip (current-input-port)))
  (println sum))

(define (in-parse-mpairs [ip (current-input-port)])
  (define (bytes->number b)
    (string->number (bytes->string/utf-8 b)))
  
  (match (regexp-match MUL-REGEX ip)
    [#f empty-stream]
    [(list _ a b)
     (stream-cons (cons (bytes->number a) (bytes->number b))
                  (in-parse-mpairs ip))]))

(module+ test
  (define-simple-check (check-parse-stream input-str expected)
    (define ip (open-input-string input-str))
    (check-equal? (stream->list (in-parse-mpairs ip)) expected))

  (check-parse-stream "" mps-mt)
  (check-parse-stream "mul(2,)" mps-mt)
  (check-parse-stream "mul(,)" mps-mt)
  (check-parse-stream "2387574asrkgas" mps-mt)
  (check-parse-stream "mul(2,4)" mps-2*4)
  (check-parse-stream "xmul(2,4)%&mul[3,7]!@^do"
                      mps-2*4)
  (check-parse-stream "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]"
                      mps-2*4/5*5))

;; InputPort -> Nat
(define (get-sum/ip ip [ipm in-parse-mpairs])
  (for/sum ([mp (ipm ip)])
    (get-sum/mpair mp)))

;; MulPair -> Nat
(define (get-sum/mpair mp)
  (* (car mp) (cdr mp)))

(module+ test
  (define-simple-check (check-gsi input-str expected)
    (define ip (open-input-string input-str))
    (check-equal? (get-sum/ip ip) expected))

  (define-simple-check (check-gsi/d input-str expected)
    (define ip (open-input-string input-str))
    (check-equal? (get-sum/ip ip in-parse-mpairs/dont) expected))

  (check-gsi "" 0)
  (check-gsi "mul(2,4)" 8)
  (check-gsi "xmul(2,4)%&mul[3,7]!@^do" 8)
  (check-gsi "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]" 33)

  (check-gsi/d "" 0)
  (check-gsi/d "mul(2,4)" 8)
  (check-gsi/d "xmul(2,4)%&mul[3,7]!@^do" 8)
  (check-gsi/d "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]" 33)
  (check-gsi/d "don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
               40)
  (check-gsi/d "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
               48))

(define (part-b)
  (define sum (get-sum/ip (current-input-port)
                          in-parse-mpairs/dont))
  (println sum))

(define (in-parse-mpairs/dont [ip (current-input-port)]
                              [enabled? #t])
  (define (bytes->number b)
    (string->number (bytes->string/utf-8 b)))
  
  (match (regexp-match MUL-DO/NT-REGEX ip)
    [#f empty-stream]
    [(list (? (curry equal? DO-BSTR)) #f #f)
     (in-parse-mpairs/dont ip #t)]
    [(list (? (curry equal? DONT-BSTR)) #f #f)
     (in-parse-mpairs/dont ip #f)]
    [(list _ a b)
     (if enabled?
         (stream-cons (cons (bytes->number a) (bytes->number b))
                      (in-parse-mpairs/dont ip enabled?))
         (in-parse-mpairs/dont ip enabled?))]))

(module+ test
  (define-simple-check (check-ipmd input-str expected)
    (define ip (open-input-string input-str))
    (check-equal? (stream->list (in-parse-mpairs/dont ip))
                  expected))

  (check-ipmd "^don't()_mul(5,5)+m" mps-mt)
  (check-ipmd "^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
              '((8 . 5))))

(command-line
 #:program "1"
 #:once-each
 [("-a" "--a") "Run part a entry point"
               (part-a)]
 [("-b" "--b") "Run part a entry point"
               (part-b)])

