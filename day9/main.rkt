#lang typed/racket

(require pfds/deque/real-time

         typed/racket/stream)

(require/typed profile
  [profile-thunk (-> (-> Any) Any)])

(module+ test
  (require typed/rackunit))

;; A File needs to have its ID, count.

(struct free-space ([amt : Natural])
  #:prefab #:type-name FreeSpace)

(struct used-space ([id : Natural] [amt : Natural])
  #:prefab #:type-name UsedSpace)

;; A Space is one of:
;; - (struct free-space Natural)
;; - (struct used-space Natural Natural)
(define-type Space (U FreeSpace UsedSpace))

;; Have the right to pry into Space.
(define-type FilesDE (Deque Space))

(: num-files/fde (-> FilesDE Natural))
(define (num-files/fde fde)
  (: num-files/space (-> Space Natural))
  (define (num-files/space s)
    (match s
      [(free-space _) 0]
      [(used-space _ amt) amt]))

  (foldr (λ ([acc : Natural] [curr : Space]) (+ acc (num-files/space curr)))
         0
         fde))

(: part-a (-> Void))
(define (part-a)
  (define fde : FilesDE (parse))
  (define checksum-num : Natural (checksum/fde fde))
  (println checksum-num))

(define-type UOF (U 'used 'free))

(: parse (->* () (Input-Port) FilesDE))
(define (parse [ip (current-input-port)])
  (for/fold : FilesDE
            ([used-or-free : UOF 'used]
             [curr-id : Natural 0]
             [out : FilesDE (deque)]
             #:result out)
            ([char (in-input-port-chars ip)])
    #:break (equal? char #\newline)
    (match used-or-free
      ['used
       (values 'free (add1 curr-id)
               (enqueue (used-space curr-id (char->number char)) out))]
      ['free
       (values 'used curr-id
               (enqueue (free-space (char->number char)) out))])))

(: checksum/fde (-> FilesDE Natural))
(define (checksum/fde fde)
  (define num-files : Natural (num-files/fde fde))
  (for/sum : Natural
           ([curr-file-id : Natural (in-compressed-fs fde num-files)]
            [curr-pos : Natural (in-naturals)])
    (* curr-pos curr-file-id)))

;; TERMINATION: When there are no more files to compress. Take it as a given that that will happen...
(: in-compressed-fs (-> FilesDE Natural [Sequenceof Natural]))
(define (in-compressed-fs fde num-files-to-compress)
  (cond
    [(zero? num-files-to-compress)
     empty-sequence]
    [else
     (match-define (cons hd tl) (head+tail fde))
     (match hd
       [(free-space amt)
        (match-define (cons fid-list new-tl)
          (get-fid-from-back tl (min amt num-files-to-compress)))
        (in-sequences (in-list fid-list)
                      (in-compressed-fs new-tl (cast (- num-files-to-compress (length fid-list)) Natural)))]
       [(used-space f-id amt)
        (in-sequences (in-repeat f-id amt)
                      (in-compressed-fs tl (cast (- num-files-to-compress amt) Natural)))])]))

(module+ test
  (: check-icf (-> String Natural [Listof Natural] Any))
  (define (check-icf input-str num-files-to-compress expected-as-list)
    (define fde (parse (open-input-string input-str)))
    (check-equal? (sequence->list (in-compressed-fs fde num-files-to-compress))
                  expected-as-list))

  (check-icf "2333" 0 '())
  (check-icf "2333" 4 '(0 0 1 1))
  (check-icf "2333" 5 '(0 0 1 1 1))
  (check-icf "23101010" 5 '(0 0 3 2 1))
  (check-icf "2333133121414131402" 28
             '(0 0 9 9 8 1 1 1 8 8 8
                 2 7 7 7 3 3 3 6 4 4
                 6 5 5 5 5 6 6)))



; Produces the list of given amt of file IDs from the back of the
; deque, in the order to fill the free space, and also produces the
; new deque. Assumes non-empty deque.
(: get-fid-from-back (-> FilesDE Natural [Pairof [Listof Natural] FilesDE]))
(define (get-fid-from-back fde amt)
  (: help (-> FilesDE Natural [Listof Natural]
              [Pairof [Listof Natural] FilesDE]))
  (define (help fde amt curr-fid-lst)
    (match-define (cons lt it) (last+init fde))
    (match lt
      [(free-space _)
       (help it amt curr-fid-lst)]
      [(used-space fid num-used)
       (define num-to-transfer (min amt num-used))
       (define new-fid-lst
         (append curr-fid-lst
                 (build-list num-to-transfer (λ (_) fid))))
       (define rem-amt (- amt num-to-transfer))
       (define rem-num-used (- num-used num-to-transfer))
       (cond
         [(> rem-amt 0)
          (help it rem-amt new-fid-lst)]
         [(> rem-num-used 0)
          (cons new-fid-lst (enqueue (used-space fid rem-num-used) it))]
         [else
          (cons new-fid-lst it)])]))

  (help fde amt '()))

(module+ test
  (: check-gffb (-> FilesDE Natural [Listof Natural] [Listof Space] Any))
  (define (check-gffb fde amt exp-fids exp-as-list)
    (define out : [Pair [Listof Natural] FilesDE]
      (get-fid-from-back fde amt))

    (check-equal? (car out) exp-fids)
    (check-equal? (deque->list (cdr out))
                  exp-as-list))

  (check-gffb (deque (used-space 1 3) (free-space 3))
              3
              '(1 1 1)
              (list))
  (check-gffb (deque (used-space 1 2) (free-space 3)
                     (used-space 2 3) (free-space 3))
              3
              '(2 2 2)
              (list (used-space 1 2) (free-space 3)))
  (check-gffb (deque (used-space 1 1) (free-space 0)
                     (used-space 2 1) (free-space 0)
                     (used-space 3 1) (free-space 0))
              3
              '(3 2 1)
              (list)))

;; Deque idea isn't going to work this time round...
;; Also, it's pretty slow...

(: part-b (-> Void))
(define (part-b)
  (error "Not done."))



(: char->number (-> Char Natural))
(define (char->number char)
  (cast (- (char->integer char) 48)
        Natural))


(: in-repeat : (All (X) X Natural -> [Sequenceof X]))
(define (in-repeat x amt)
  (cond
    [(zero? amt)
     empty-stream]
    [(positive? amt)
     (stream-cons x (in-repeat x (sub1 amt)))]))

(command-line
 #:program "1"
 #:once-each
 [("-a" "--a") "Run part a entry point"
               (profile-thunk (λ () (part-a)))]
 [("-b" "--b") "Run part a entry point"
               (part-b)])
