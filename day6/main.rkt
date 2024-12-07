#lang racket

(module+ examples
  (provide tps-no-inter tps-no-interx2
           tps-inter
           tps-loop
           tps-loop-big
           tps-paral+inter))

(module+ test
  (require rackunit
           (submod ".." examples)))

;; A Map is a (map Dir Coord [Hashof Nat [Setof Nat]] [Hashof Nat [Setof Nat]])
;; Represents where the guard is initially facing, the guard's initial
;; position, and the set of coordinates where the obstructions are on. The set of coordinates is a mapping from x coordinate to y coordinates, and y coordinate to x coordinates. This is for performance reasons. Map also had width and height.
(struct gmap [dir guard-pos w h x->ys y->xs] #:prefab)

;; [Listof String] -> Map
(define (make-gmap los)
  (parse (open-input-string (string-append (string-join los "\n") "\n"))))

(module+ test
  (check-equal? (make-gmap '("#.."
                             "..#"
                             "^.."))
                (gmap 'n '(0 . 2)
                      3 3
                      (hash 0 (set 0)
                            2 (set 1))
                      (hash 0 (set 0)
                            1 (set 2)))))

;; A Dir is one of ['n | 's | 'e | 'w]
;; Represents which direction the guard is facing.

(define (char->guard-dir c)
  (match c
    [#\^ 'n]
    [#\v 's]
    [#\> 'e]
    [#\< 'w]))

(define (rot-dir-cw dir)
  (match dir
    ['n 'e]
    ['e 's]
    ['s 'w]
    ['w 'n]))

;; A Coord is a [Pairof Nat Nat]
;; Anyone has to right to deconstruct this.
;; Left of pair is the x-value, right of pair is the y-value.
;; Rightwards is increase in x, downwards is increase in y.
;; Top left position is (0,0)

;; Absolute difference between the two given Coords.
(define (coord-diff a b)
  (+ (abs (- (car a) (car b)))
     (abs (- (cdr a) (cdr b)))))
(module+ test
  (check-equal? (coord-diff '(1 . 3) '(1 . 1)) 2)
  (check-equal? (coord-diff '(2 . 7) '(5 . 7)) 3)
  (check-equal? (coord-diff '(6 . 6) '(6 . 6)) 0))

(define (both-vertical? a1 a2 b1 b2)
  (and (= (car a1) (car a2))
       (= (car b1) (car b2))))

(define (both-horizontal? a1 a2 b1 b2)
  (and (= (cdr a1) (cdr a2))
       (= (cdr b1) (cdr b2))))

;; Nat Nat Nat Nat -> Boolean
(define (overlap a1 a2 b1 b2)
  (define a-s (min a1 a2))
  (define a-l (max a1 a2))
  (define b-s (min b1 b2))
  (define b-l (max b1 b2))
  (cond
    [(<= a-s b-s)
     (min (add1 (- a-l b-s)) (add1 (- b-l b-s)))]
    [else
     (min (add1 (- b-l a-s)) (add1 (- a-l a-s)))]))
(module+ test
  (check-equal? (overlap 1 3 2 4) 2)
  (check-equal? (overlap 2 4 1 3) 2)
  (check-equal? (overlap 1 6 3 5) 3)
  (check-equal? (overlap 0 2 1 3) 2))

;; Assumes that both are perpendicular and not connected.
(define (has-cross? a1 a2 b1 b2)
  (match-define (list vertical-x y-s y-l horizontal-y x-s x-l)
    (get-verhor a1 a2 b1 b2))
  (and (<= x-s vertical-x x-l)
       (<= y-s horizontal-y y-l)))
(define (get-verhor a1 a2 b1 b2)
  (if (= (car a1) (car a2))
      (append (list (car a1))
              (sort (list (cdr a1) (cdr a2)) <)
              (list (cdr b1))
              (sort (list (car b1) (car b2)) <))
      (append (list (car b1))
              (sort (list (cdr b1) (cdr b2)) <)
              (list (cdr a1))
              (sort (list (car a1) (car a2)) <))))

(module+ test
  (check-true (has-cross? '(0 . 1) '(2 . 1) '(1 . 0) '(1 . 2)))
  (check-true (has-cross? '(0 . 1) '(2 . 1) '(2 . 0) '(2 . 2)))
  (check-true (has-cross? '(0 . 1) '(2 . 1) '(0 . 0) '(0 . 2)))
  (check-true (has-cross? '(1 . 0) '(1 . 2) '(0 . 1) '(2 . 1)))
  (check-false (has-cross? '(0 . 1) '(2 . 1) '(3 . 0) '(3 . 2)))
  (check-false (has-cross? '(0 . 2) '(0 . 1) '(1 . 1) '(1 . 2))))

(define (get-intersections a1 a2 b1 b2)
  (cond
    [(both-vertical? a1 a2 b1 b2)
     (if (equal? (car a1) (car b1))
         (overlap (cdr a1) (cdr a2) (cdr b1) (cdr b2))
         0)]
    [(both-horizontal? a1 a2 b1 b2)
     (if (equal? (cdr a1) (cdr b1))
         (overlap (car a1) (car a2) (car b1) (car b2))
         0)]
    [else
     (if (has-cross? a1 a2 b1 b2) 1 0)]))
(module+ test
  (check-equal? (get-intersections '(0 . 2) '(0 . 1) '(1 . 1) '(1 . 2))
                0)

  ; Corner
  (check-equal? (get-intersections '(1 . 2) '(1 . 0) '(1 . 0) '(2 . 0))
                1)
  (check-equal? (get-intersections '(0 . 1) '(2 . 1) '(2 . 1) '(2 . 0))
                1)
  (check-equal? (get-intersections '(1 . 0) '(1 . 1) '(1 . 1) '(4 . 1))
                1)
  (check-equal? (get-intersections '(2 . 1) '(2 . 0) '(2 . 0) '(1 . 0))
                1)
  (check-equal? (get-intersections '(2 . 0) '(1 . 0) '(1 . 0) '(1 . 1))
                1)

  ; Cross
  (check-equal? (get-intersections '(0 . 1) '(2 . 1) '(1 . 0) '(1 . 2))
                1)

  ; T
  (check-equal? (get-intersections '(0 . 1) '(2 . 1) '(2 . 0) '(2 . 2))
                1)
  (check-equal? (get-intersections '(0 . 1) '(2 . 1) '(1 . 0) '(1 . 1))
                1)
  (check-equal? (get-intersections '(2 . 1) '(2 . 0) '(1 . 1) '(4 . 1))
                1)

  ; Parallel
  (check-equal? (get-intersections '(2 . 1) '(0 . 1) '(1 . 1) '(3 . 1))
                2)
  (check-equal? (get-intersections '(1 . 1) '(3 . 1) '(2 . 1) '(0 . 1))
                2)
  (check-equal? (get-intersections '(0 . 1) '(2 . 1) '(1 . 1) '(4 . 1))
                2))

;; A TurningPoints is a [NEListof Coord]
;; Represents where the guard stops to turn, includes the guard's
;; initial position and final coordinates.

(module+ examples
  (define tps-no-inter '((0 . 2) (0 . 1) (1 . 1) (1 . 2)))
  (define tps-no-interx2 '((0 . 4) (0 . 2) (2 . 2) (2 . 4)))
  (define tps-inter '((1 . 2) (1 . 0) (2 . 0) (2 . 1) (0 . 1)))
  (define tps-loop '((0 . 1) (2 . 1) (2 . 0) (1 . 0) (1 . 1) (4 . 1)))
  (define tps-loop-big '((0 . 1) (3 . 1) (3 . 0) (1 . 0) (1 . 1) (5 . 1)))
  (define tps-paral+inter '((1 . 1) (3 . 1) (3 . 2) (0 . 2) (0 . 0) (2 . 0) (2 . 1) (0 . 1))))

(define num-points length)

(define (total-dist/tps tps)
  (for/fold ([s 0]
             [prev-tp (first tps)]
             #:result s)
            ([curr-tp (in-list (rest tps))])
    (values (+ s 1 (coord-diff prev-tp curr-tp))
            curr-tp)))
(module+ test
  (check-equal? (total-dist/tps tps-no-inter) 6)
  (check-equal? (total-dist/tps tps-no-interx2) 9)
  (check-equal? (total-dist/tps tps-inter) 10)
  (check-equal? (total-dist/tps tps-loop) 13)
  (check-equal? (total-dist/tps tps-loop-big) 16)
  (check-equal? (total-dist/tps tps-paral+inter) 20))

(define (in-first-nerest lox)
  (cond
    [(empty? (rest lox))
     empty-stream]
    [(cons? (rest lox))
     (stream-cons (values (first lox) lox)
                  (in-first-nerest (rest lox)))]))
(module+ test
  (define-simple-check (check-ifn lox expected)
    (for ([(x rx) (in-first-nerest lox)]
          [p (in-list expected)])
      (check-equal? (list x rx)
                    p)))

  (check-ifn '(1 2) '((1 (1 2)) (2 (2)))))

;; Includes parallels.
;; O(n^2) time complexity.
(define (total-intersections tps)
  (newline) (println "NEW")
  (for/fold ([outer-num 0]
             [prev-a (first tps)]
             #:result outer-num)
            ([(curr-a remaining-tps) (in-first-nerest (rest tps))])
    (values
     (+ outer-num
        (for/fold ([inner-num 0]
                   [prev-b (first remaining-tps)]
                   #:result inner-num)
                  ([curr-b (in-list (rest remaining-tps))])
          
          (printf "~a," (get-intersections prev-a curr-a prev-b curr-b))
          (printf "(~a,~a,~a,~a)"
                  prev-a curr-a prev-b curr-b)
          (values (+ inner-num
                     (get-intersections prev-a curr-a prev-b curr-b))
                  curr-b)))
     curr-a)))
(module+ test
  (check-equal? (total-intersections tps-no-inter) 2)
  (check-equal? (total-intersections tps-no-interx2) 2)
  (check-equal? (total-intersections tps-inter) 4)
  (check-equal? (total-intersections tps-loop) 6)
  (check-equal? (total-intersections tps-loop-big) 7)
  (check-equal? (total-intersections tps-paral+inter) 9))

(define (part-a)
  (define initial-m (parse))
  (define tps (map->turning-points initial-m))
  (define num-distinct-poss (num-distinct-poss/tps tps))
  (println num-distinct-poss))

(define (parse [ip (current-input-port)])
  (for/fold ([guard-dir #f]
             [guard-pos #f]
             [w #f] [h #f]
             [x->ys (hash)] [y->xs (hash)]
             [curr-x 0] [curr-y 0]
             #:result (gmap guard-dir guard-pos w h x->ys y->xs))
            ([c (in-port read-char ip)]
             #:do [(define curr-pos (cons curr-x curr-y))])
    (case c
      [(#\newline)
       (values guard-dir guard-pos
               curr-x (add1 curr-y)
               x->ys y->xs
               0 (add1 curr-y))]
      [(#\#)
       (values guard-dir guard-pos
               w h
               (hash-update x->ys curr-x
                            (λ (s) (set-add s curr-y)) (set))
               (hash-update y->xs curr-y
                            (λ (s) (set-add s curr-x)) (set))
               (add1 curr-x) curr-y)]
      [(#\^ #\> #\v #\<)
       (values (char->guard-dir c) curr-pos w h x->ys y->xs
               (add1 curr-x) curr-y)]
      [(#\.)
       (values guard-dir guard-pos w h x->ys y->xs
               (add1 curr-x) curr-y)]
      [else (error "Unrecognized char:" c)])))
(module+ test
  (define-simple-check (check-parse input-str expected)
    (define ip (open-input-string input-str))
    (check-equal? (parse ip) expected))

  (check-parse "#..^\n"
               (gmap 'n '(3 . 0)
                     4 1
                     (hash 0 (set 0))
                     (hash 0 (set 0))))
  (check-parse ".#.\n#.#\n.<.\n"
               (gmap 'w '(1 . 2)
                     3 3
                     (hash 0 (set 1)
                           1 (set 0)
                           2 (set 1))
                     (hash 0 (set 1)
                           1 (set 0 2)))))

;; Map -> TurningPoints
(define (map->turning-points m)
  (match-define (gmap gdir0 gpos0 w h x->ys y->xs) m)
  (let loop ([gdir gdir0] [gpos gpos0] [rv-tps (list gpos0)])
    (match-define (cons next-pos edge?)
      (get-pos-bf-obstruction gdir gpos w h x->ys y->xs))
    (define new-rv-tps (cons next-pos rv-tps))
    (if edge?
        (reverse new-rv-tps)
        (loop (rot-dir-cw gdir) next-pos new-rv-tps))))
(module+ test
  (check-equal? (map->turning-points
                 (make-gmap '("#.."
                              "..#"
                              "^..")))
                '((0 . 2) (0 . 1) (1 . 1) (1 . 2))))

;; Dir Coord Nat Nat [Hashof Nat [Setof Nat]] [Hashof Nat [Setof Nat]]
;; -> [#f | Coord]
;; If the guard with the given direction and position keeps walking
;; straight, if it will run into an obstruction, output the position
;; before the obstruction, or output false if no obstruction.
(define (get-pos-bf-obstruction dir pos w h x->ys y->xs)
  (match-define (cons x y) pos)
  (match dir
    ['n
     (define ys (hash-ref x->ys x (set)))
     (define new-y (and (get-closest-to y ys <)))
     (if new-y
         (cons (cons x (+ new-y 1)) #f)
         (cons (cons x 0) #t))]
    ['s
     (define ys (hash-ref x->ys x (set)))
     (define new-y (get-closest-to y ys >))
     (if new-y
         (cons (cons x (+ new-y -1)) #f)
         (cons (cons x (sub1 h)) #t))]
    ['e
     (define xs (hash-ref y->xs y (set)))
     (define new-x (get-closest-to x xs >))
     (if new-x
         (cons (cons (+ new-x -1) y) #f)
         (cons (cons (sub1 w) y) #t))]
    ['w
     (define xs (hash-ref y->xs y (set)))
     (define new-x (get-closest-to x xs <))
     (if new-x
         (cons (cons (+ new-x 1) y) #f)
         (cons (cons 0 y) #t))]))
(module+ test
  (check-equal? (get-pos-bf-obstruction 'n '(1 . 5) 5 5
                                        (hash 1 (set 2))
                                        (hash 2 (set 1)))
                '((1 . 3) . #f))
  (check-equal? (get-pos-bf-obstruction 'n '(0 . 2) 3 3
                                        (hash 0 (set 0))
                                        (hash 0 (set 0)))
                '((0 . 1) . #f))
  (check-equal? (get-pos-bf-obstruction 'n '(1 . 2) 6 6
                                        (hash 2 (set 5))
                                        (hash 5 (set 2)))
                '((1 . 0) . #t))
  (check-equal? (get-pos-bf-obstruction 's '(5 . 3) 7 7
                                        (hash 5 (set 6))
                                        (hash 6 (set 5)))
                '((5 . 5) . #f))
  (check-equal? (get-pos-bf-obstruction 'e '(2 . 7) 8 8
                                        (hash 7 (set 7))
                                        (hash 7 (set 7)))
                '((6 . 7) . #f))
  (check-equal? (get-pos-bf-obstruction 'w '(11 . 0) 12 12
                                        (hash 0 (set 0))
                                        (hash 0 (set 0)))
                '((1 . 0) . #f)))
;; {X} X [Setof X] [X X -> Boolean] -> [#f X]
;; Gets the element in the given list closest to the given value with
;; the given comparator, and the value and the given x passed into the
;; comp must be true. Outputs #f if no such value exists.
(define (get-closest-to x0 sox comp)
  (define slox (sort (set->list sox) comp))
  (for/last ([x (in-list slox)]
             #:when (comp x x0))
    x))
(module+ test
  ; Should have tests for things other than nats... whatever.
  (check-equal? (get-closest-to 3 (set 1 2 3 4 5) <) 2)
  (check-equal? (get-closest-to 3 (set 1 2 3 4 5) >) 4)
  (check-equal? (get-closest-to 3 (set 1 2 4 5) <) 2)
  (check-equal? (get-closest-to 3 (set 1 2 4 5) >) 4)
  (check-false (get-closest-to 5 (set 1 2 4 5) >))
  (check-false (get-closest-to 1 (set 1 2 4 5) <)))

;; TurningPoints -> Nat
(define (num-distinct-poss/tps tps)
  (for/fold ([s 0]
             [seen-rv-tps (list (first tps))]
             [prev-tp (first tps)]
             #:result s)
            ([curr-tp (in-list (rest tps))])
    (values (+ s (num-distinct-poss/tp prev-tp curr-tp seen-rv-tps))
            (cons curr-tp seen-rv-tps)
            curr-tp)))
;; Coord Coord TurningPoints (Reversed) -> Nat
(define (num-distinct-poss/tp a b seen-rv-tps)
  (for/sum ([x (in-inclusive-range (car a) (car b))]
            [y (in-inclusive-range (cdr a) (cdr b))])
    (if (tps-contains-coord? seen-rv-tps x y) 1 0)))
(define (tps-contains-coord? tps x y)
  (for/fold ([prev-tp (first tps)]
             [found? #f]
             #:result found?)
            ([curr-tp (in-list (rest tps))])
    #:break found?
    (values curr-tp
            (coord-range-contains-coord? prev-tp curr-tp x y))))
(define (coord-range-contains-coord? prev-tp curr-tp x y)
  (and (or (<= (car prev-tp) x (car curr-tp))
           (>= (car prev-tp) x (car curr-tp)))
       (or (<= (cdr prev-tp) y (cdr curr-tp))
           (>= (cdr prev-tp) y (cdr curr-tp)))))
(module+ test
  ; No intersections
  (check-equal? (num-distinct-poss/tps tps-no-inter) 4)

  ; Cross intersections
  (check-equal? (num-distinct-poss/tps tps-inter) 6)

  ; Parallel intersections
  (check-equal? (num-distinct-poss/tps tps-loop) 7)
  (check-equal? (num-distinct-poss/tps tps-loop-big) 9)

  ; Parallel + cross intersections
  (check-equal? (num-distinct-poss/tps tps-paral+inter) 11))

(define (part-b)
  (error "Not done."))

(command-line
 #:program "1"
 #:once-each
 [("-a" "--a") "Run part a entry point"
               (part-a)]
 [("-b" "--b") "Run part a entry point"
               (part-b)])

