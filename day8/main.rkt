#lang racket

(module+ test
  (require rackunit))

;; A Freq is one of:
;; - [0-9]
;; - [a-z]
;; - [A-Z]

;; A Map is one of [NEVectorof [NEVectorof [Freq | #f]]]
;; Where all the lengths of the inner vectors are the same, and form the width of the map.
;; The length of the outer vector is the height of the map.

;; Map Pos -> [Freq | #f]
(define (at-pos/map m pos)
  (match-define (cons r c) pos)
  (vector-ref (vector-ref m r)
              c))

;; A Pos is a (cons Nat Nat), where the left hand side is the row,
;; index of the outer vector, and the left hand side is the col, index
;; of the inner vector.

;; Pos Pos -> Pos
(define (pos-add a b)
  (cons (+ (car a) (car b))
        (+ (cdr a) (cdr b))))

;; Pos Pos -> Pos
(define (pos-sub a b)
  (cons (- (car a) (car b))
        (- (cdr a) (cdr b))))

;; Pos Nat Nat -> Boolean
;; Is the given pos within the given height and width?
(define (within-bounds? p h w)
  (match-define (cons r c) p)
  (and (<= 0 r) (< r h)
       (<= 0 c) (< c w)))

(define (part-a)
  (define m (parse))
  (define num-antinodes (num-antinodes/map m))
  (println num-antinodes))

;; InputPort? -> Map
(define (parse [ip (current-input-port)])
  (for/vector ([line (in-lines ip)]
        [r (in-naturals)])
    (for/vector ([char line]
               [c (in-naturals)])
      (if (equal? char #\.)
          #f
          char))))

;; Map -> Nat
(define (num-antinodes/map m [pp->a pos-pair->antinodes])
  (define antinodes (antinodes/map m pp->a))
  (set-count antinodes))

;; Map -> [Setof Freq]
(define (antinodes/map m [pp->a pos-pair->antinodes])
  (for/fold ([antinodes-set (set)])
            ([antinodes (in-antinodes m pp->a)])
    (set-union antinodes-set
               (list->set antinodes))))

;; Map [[Listof Pos] Nat Nat -> [Listof Pos]] ->
;; [Sequenceof [Listof Pos]] (where list of pos has length of at most 2)
(define (in-antinodes m [pp->a pos-pair->antinodes])
  (define mh (vector-length m))
  (define mw (vector-length (vector-ref m 0)))

  (sequence-map
   (λ (pos-pair) (pp->a pos-pair mh mw))
   (sequence-filter
    (λ (pos-pair) (same-frequency?/pos-pair m pos-pair))
    (in-combinations-pos mh mw))))

;; Nat Nat Nat Nat -> [Streamof Pos]
;; This doesn't have to be a stream because in-combinations-pos wants all of them at once, eh, whatever.
(define (in-pos-range sh sw eh ew)
  (define (help curr-h curr-w)
    (cond
      [(and (= curr-h eh))
       empty-stream]
      [(= curr-w ew)
       (help (add1 curr-h) 0)]
      [else
       (stream-cons (cons curr-h curr-w)
                    (help curr-h (add1 curr-w)))]))

  (help sh sw))

;; Nat Nat -> [Sequenceof (list Pos Pos)]
(define (in-combinations-pos mh mw)
  (in-combinations (stream->list (in-pos-range 0 0 mh mw))
                   2))

;; Map (list Pos Pos) -> Boolean
(define (same-frequency?/pos-pair m pos-pair)
  (match-define (list a b) pos-pair)
  (define fa (at-pos/map m a))
  (define fb (at-pos/map m b))
  (and fa fb (equal? fa fb)))

;; (list Pos Pos) Nat Nat -> [Listof Pos]
;; Only keeps antinodes if they are within the given bounds.
(define (pos-pair->harmonic-antinodes pos-pair mh mw)
  (define (help/dir x delta acc pos-op)
    (if (within-bounds? x mh mw)
        (help/dir (pos-op x delta) delta
                  (cons x acc)
                  pos-op)
        acc))

  (match-define (list xpos ypos) pos-pair)
  (define delta (pos-sub ypos xpos))
  
  (define w/sub-dir (help/dir xpos delta '() pos-sub))
  (define w/add-dir (help/dir ypos delta w/sub-dir pos-add))
  w/add-dir)

;; (list Pos Pos) Nat Nat -> [Listof Pos]
;; Where the resulting list has at most length 2.
;; Only keeps antinodes if they are within the given bounds.
(define (pos-pair->antinodes pos-pair mh mw)
  (match-define (list xpos ypos) pos-pair)
  (define delta (pos-sub ypos xpos))
  (define antinode1 (pos-sub xpos delta))
  (define antinode2 (pos-add ypos delta))
  (filter (λ (an) (within-bounds? an mh mw))
          (list antinode1 antinode2)))
(module+ test
  (check-equal? (pos-pair->antinodes '((5 . 5) (6 . 6)) 100 100)
                '((4 . 4) (7 . 7)))
  (check-equal? (pos-pair->antinodes '((5 . 5) (6 . 6)) 100 7)
                '((4 . 4)))
  (check-equal? (pos-pair->antinodes '((0 . 0) (1 . 1)) 100 3)
                '((2 . 2)))
  (check-equal? (pos-pair->antinodes '((0 . 0) (1 . 1)) 100 2)
                '())
  (check-equal? (pos-pair->antinodes '((4 . 5) (6 . 6)) 100 100)
                '((2 . 4) (8 . 7)))
  (check-equal? (pos-pair->antinodes '((6 . 6) (4 . 5)) 100 100)
                '((8 . 7) (2 . 4)))
  (check-equal? (pos-pair->antinodes '((6 . 6) (4 . 5)) 8 100)
                '((2 . 4)))
  (check-equal? (pos-pair->antinodes '((6 . 6) (4 . 5)) 100 100)
                '((8 . 7) (2 . 4)))
  (check-equal? (pos-pair->antinodes '((6 . 6) (7 . 8)) 100 100)
                '((5 . 4) (8 . 10)))
  (check-equal? (pos-pair->antinodes '((7 . 8) (6 . 6)) 100 100)
                '((8 . 10) (5 . 4))))

(define (part-b)
  (define m (parse))
  (define num-harmonic-antinodes (num-antinodes/map m pos-pair->harmonic-antinodes))
  (println num-harmonic-antinodes))

(command-line
 #:program "1"
 #:once-each
 [("-a" "--a") "Run part a entry point"
               (part-a)]
 [("-b" "--b") "Run part a entry point"
               (part-b)])
