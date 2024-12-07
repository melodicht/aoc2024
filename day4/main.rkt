#lang racket

(module+ examples
  (provide ws-mt
           ws-1x1 ws-1x2 ws-2x1
           ws-xmas-right ws-xmas-left
           ws-xmas-top ws-xmas-bottom
           ws-xmas-se ws-xmas-sw
           ws-xmas-ne ws-xmas-nw
           ws-x-mas-w ws-x-mas-e
           ws-x-mas-n ws-x-mas-s))

(module+ test
  (require rackunit
           (submod ".." examples)))

(define (string->vector s) (list->vector (string->list s)))
(module+ test
  (check-equal? (string->vector "XMAS")
                #(#\X #\M #\A #\S)))

(define XMAS (list->vector (string->list "XMAS")))
(define MAS (list->vector (string->list "MAS")))

;; A WordSearch ia [Vectorof [NEVectorof Char]]

;; A Word is a [NEVectorof Char]

(module+ examples
  (define ws-mt #())

  ; AxB is rows x columns.
  (define ws-1x1 #(#(#\A)))
  (define ws-1x2 #(#(#\X #\Y)))
  (define ws-2x1 #(#(#\U) #(#\V)))

  ; XMAS all directions
  (define ws-xmas-right (make-ws '("XMAS")))
  (define ws-xmas-left (make-ws '("SAMX")))
  (define ws-xmas-bottom (make-ws '("X" "M" "A" "S")))
  (define ws-xmas-top (make-ws '("S" "A" "M" "X")))

  ; Compass directions
  (define ws-xmas-se (make-ws '("X..."
                                ".M.."
                                "..A."
                                "...S")))
  (define ws-xmas-sw (make-ws '("...X"
                                "..M."
                                ".A.."
                                "S...")))
  (define ws-xmas-ne (make-ws '("...S"
                                "..A."
                                ".M.."
                                "X...")))
  (define ws-xmas-nw (make-ws '("S..."
                                ".A.."
                                "..M."
                                "...X")))

  (define ws-x-mas-w (make-ws '("M.S"
                                ".A."
                                "M.S")))
  (define ws-x-mas-e (make-ws '("S.M"
                                ".A."
                                "S.M")))
  (define ws-x-mas-n (make-ws '("M.M"
                                ".A."
                                "S.S")))
  (define ws-x-mas-s (make-ws '("S.S"
                                ".A."
                                "M.M"))))

;; WordSearch Nat Nat -> Char
;; Gets the character at the given row and column position of the
;; given word search.
;; Assumes that the given row and column are within the bounds of the
;; given word search.
(define (ws-at ws r c)
  (vector-ref (vector-ref ws r) c))
(module+ test
  (check-equal? (ws-at ws-1x1 0 0) #\A)
  (check-equal? (ws-at ws-1x2 0 0) #\X)
  (check-equal? (ws-at ws-1x2 0 1) #\Y)
  (check-equal? (ws-at ws-2x1 1 0) #\V))

;; WordSearch -> [Valuesof Nat Nat]
;; Outputs the number of rows and columns of the given word search, in
;; that order.
;; Assumes that the given word search is non empty.
(define (num-rows-cols/ws ws)
  (values (vector-length ws) (vector-length (vector-ref ws 0))))
(module+ test
  (define-simple-check (check-nrc ws er ec)
    (define-values (nr nc) (num-rows-cols/ws ws))
    (check-equal? (list nr nc) (list er ec)))

  (check-nrc ws-1x1 1 1)
  (check-nrc ws-1x2 1 2)
  (check-nrc ws-2x1 2 1))

;; [Listof String] -> WordSearch
;; A convenience constructor that makes word searches from the given
;; list of strings.
(define (make-ws los)
  (for/vector #:length (length los)
    ([s (in-list los)])
    (list->vector (string->list s))))
(module+ test
  (check-equal? (make-ws '("A")) ws-1x1)
  (check-equal? (make-ws '("XY")) ws-1x2)
  (check-equal? (make-ws '("U" "V")) ws-2x1))


(define (part-a)
  (define ws (parse))
  (define word-count (word-count/ws ws XMAS))
  ; For each char, if it is an X, check all 8 directions. If so, add 1.
  (println word-count))

(define (parse [ip (current-input-port)])
  (for/vector ([line (in-lines ip)])
    (string->vector line)))
(module+ test
  (define-simple-check (check-parse input-str expected)
    (define ip (open-input-string input-str))
    (check-equal? (parse ip) expected))
  
  (check-parse "" ws-mt)
  (check-parse "A" ws-1x1)
  (check-parse "A\n" ws-1x1)
  (check-parse "XY" ws-1x2)
  (check-parse "U\nV\n" ws-2x1))

;; WordSearch Word -> Nat
(define (word-count/ws ws word
                       [trigger-pos 0]
                       [num-matches/r+c num-word-matches/r+c])
  (define trigger-c (vector-ref word trigger-pos))
  (for/sum ([vec-chars (in-vector ws)]
            [row (in-naturals)])
    (for/sum ([c (in-vector vec-chars)]
              [col (in-naturals)])
      (if (equal? c trigger-c)
          (num-matches/r+c ws word row col)
          0))))
(module+ test
  (check-equal? (word-count/ws ws-xmas-right XMAS) 1)
  (check-equal? (word-count/ws ws-xmas-left XMAS) 1)
  (check-equal? (word-count/ws ws-xmas-bottom XMAS) 1)
  (check-equal? (word-count/ws ws-xmas-top XMAS) 1)
  (check-equal? (word-count/ws ws-xmas-se XMAS) 1)
  (check-equal? (word-count/ws ws-xmas-sw XMAS) 1)
  (check-equal? (word-count/ws ws-xmas-ne XMAS) 1)
  (check-equal? (word-count/ws ws-xmas-nw XMAS) 1)

  (check-equal? (word-count/ws ws-mt XMAS) 0)
  (check-equal? (word-count/ws ws-1x1 XMAS) 0)
  (check-equal? (word-count/ws ws-1x2 XMAS) 0))

;; WordSearch Word Nat Nat -> Nat
;; Gets the number of times the given word is in the given word
;; search, where the first letter of the given word is in the given
;; row and column in the given word search.
(define (num-word-matches/r+c ws word r c)
  (define word-length (vector-length word))
  (define-values (num-rows num-cols) (num-rows-cols/ws ws))
  
  ; Int Int Int? -> [1 | 0]
  ; Produces 1 if the characters at the current row and column
  ; position (initialized to the initial r and c) matches the word in
  ; the given change in direction (dr,dc), where word-pos character in
  ; word ought to equal to the letter at current row and column in
  ; word search.
  (define (help/delta dr dc [cr r] [cc c] [word-pos 0])
    (cond
      [(>= word-pos word-length) 1]
      [(or (< cr 0) (< cc 0) (>= cr num-rows) (>= cc num-cols)) 0]
      [(equal? (ws-at ws cr cc) (vector-ref word word-pos))
       (help/delta dr dc (+ cr dr) (+ cc dc) (add1 word-pos))]
      [else 0]))
  (+ (help/delta 1 0) (help/delta -1 0)
     (help/delta 0 1) (help/delta 0 -1)
     (help/delta 1 1) (help/delta 1 -1)
     (help/delta -1 1) (help/delta -1 -1)))

(module+ test
  (check-equal? (num-word-matches/r+c ws-xmas-right XMAS 0 0) 1)
  (check-equal? (num-word-matches/r+c ws-xmas-left XMAS 0 3) 1)
  (check-equal? (num-word-matches/r+c ws-xmas-bottom XMAS 0 0) 1)
  (check-equal? (num-word-matches/r+c ws-xmas-top XMAS 3 0) 1)
  (check-equal? (num-word-matches/r+c ws-xmas-se XMAS 0 0) 1)
  (check-equal? (num-word-matches/r+c ws-xmas-sw XMAS 0 3) 1)
  (check-equal? (num-word-matches/r+c ws-xmas-ne XMAS 3 0) 1)
  (check-equal? (num-word-matches/r+c ws-xmas-nw XMAS 3 3) 1)

  (check-equal? (num-word-matches/r+c ws-xmas-nw XMAS 2 2) 0)
  (check-equal? (num-word-matches/r+c ws-xmas-ne XMAS 0 3) 0))

(define (part-b)
  (define ws (parse))
  (define match-count (word-count/ws ws MAS 1 num-x-matches/r+c))
  (println match-count))

;; WordSearch Word Nat Nat -> Nat
;; Gets the number of times the given word in a cross appears in the
;; given word such such that the middle character of the word is in
;; the given row and column positions. Assume that the word has an odd
;; length.
(define (num-x-matches/r+c ws word r c)
  (define word-length (vector-length word))
  (define initial-word-pos (/ (sub1 word-length) 2))
  (define-values (num-rows num-cols) (num-rows-cols/ws ws))

  (define (help/delta dr dc [cr r] [cc c] [word-pos 0])
    (cond
      [(>= word-pos word-length) #t]
      [(or (< cr 0) (< cc 0) (>= cr num-rows) (>= cc num-cols)) #f]
      [(equal? (ws-at ws cr cc) (vector-ref word word-pos))
       (help/delta dr dc (+ cr dr) (+ cc dc) (add1 word-pos))]
      [else #f]))

  (if (or (and (help/delta -1 1 (add1 r) (sub1 c)) ; East
               (help/delta 1 1 (sub1 r) (sub1 c)))
          (and (help/delta -1 -1 (add1 r) (add1 c)) ; West
               (help/delta 1 -1 (sub1 r) (add1 c)))
          (and (help/delta -1 1 (add1 r) (sub1 c)) ; North
               (help/delta -1 -1 (add1 r) (add1 c)))
          (and (help/delta 1 1 (sub1 r) (sub1 c)) ; South
               (help/delta 1 -1 (sub1 r) (add1 c))))
      1
      0))

;; N
;; -1 -1
;; 1 1

;; S
;; 1 1
;; -1 -1

;; W
;; -1 1
;; -1 1
;; E
;; 1 -1
;; 1 -1

;; Abstraction ideas: Pull out help/delta, num-matches to accept initial-word-pos and directions of interest.
(module+ test
  (check-equal? (num-x-matches/r+c ws-x-mas-w MAS 1 1) 1)
  (check-equal? (num-x-matches/r+c ws-x-mas-e MAS 1 1) 1)
  (check-equal? (num-x-matches/r+c ws-x-mas-n MAS 1 1) 1)
  (check-equal? (num-x-matches/r+c ws-x-mas-s MAS 1 1) 1)
  
  (check-equal? (num-x-matches/r+c ws-x-mas-w MAS 0 0) 0)
  (check-equal? (num-x-matches/r+c ws-x-mas-w MAS 0 0) 0))


(command-line
 #:program "1"
 #:once-each
 [("-a" "--a") "Run part a entry point"
               (part-a)]
 [("-b" "--b") "Run part a entry point"
               (part-b)])
