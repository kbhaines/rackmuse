#lang racket

(provide
 w wr dw dwr
 h hr dh dhr
 q qr dq dqr
 e er
 ;; bar barr dbar dbarr

 repeat

 mk-chord chord-notes chord-duration
 mk-note note-note note-duration

 project-chords
 project-notes

 mk-track track-name track-spans

 join

 oct+ oct-
 oct++ oct--
 oct+++ oct---

 c0 cs0 df0 d0 ds0 ef0 e0 f0 fs0 gf0 g0 gs0 af0 a0 as0 bf0 b0
 c1 cs1 df1 d1 ds1 ef1 e1 f1 fs1 gf1 g1 gs1 af1 a1 as1 bf1 b1
 c2 cs2 df2 d2 ds2 ef2 e2 f2 fs2 gf2 g2 gs2 af2 a2 as2 bf2 b2
 c3 cs3 df3 d3 ds3 ef3 e3 f3 fs3 gf3 g3 gs3 af3 a3 as3 bf3 b3
 c4 cs4 df4 d4 ds4 ef4 e4 f4 fs4 gf4 g4 gs4 af4 a4 as4 bf4 b4
 c5 cs5 df5 d5 ds5 ef5 e5 f5 fs5 gf5 g5 gs5 af5 a5 as5 bf5 b5
 c6 cs6 df6 d6 ds6 ef6 e6 f6 fs6 gf6 g6 gs6 af6 a6 as6 bf6 b6
 c7 cs7 df7 d7 ds7 ef7 e7 f7 fs7 gf7 g7 gs7 af7 a7 as7 bf7 b7
 c8
 )

(define PPQ 480)

(define w (* 4 PPQ))
(define wr (- 0 w))
(define dw (* 3 (/ w 2)))
(define dwr (- 0 dw))

(define h (* 2 PPQ))
(define hr (- 0 h))
(define dh (* 3 (/ h 2)))
(define dhr (- 0 dh))

(define q PPQ)
(define qr (- 0 q))
(define dq (* 3 (/ q 2)))
(define dqr (- 0 dq))

(define e (/ q 2))
(define er (- 0 e))

;; (define bar (* 3 q))
;; (define barr (- 0 bar))
;; (define dbar (* 2 bar))
;; (define dbarr (- 0 dbar))
;;
;; (define tacet1 (list dbar))
;; (define tacet2 (list dbarr))
;; (define tacet4 (list dbarr dbarr))

(define (repeat n . ls) (flatten (make-list n (list ls))))

(define-values
  (c0 cs0 df0 d0 ds0 ef0 e0 f0 fs0 gf0 g0 gs0 af0 a0 as0 bf0 b0
      c1 cs1 df1 d1 ds1 ef1 e1 f1 fs1 gf1 g1 gs1 af1 a1 as1 bf1 b1
      c2 cs2 df2 d2 ds2 ef2 e2 f2 fs2 gf2 g2 gs2 af2 a2 as2 bf2 b2
      c3 cs3 df3 d3 ds3 ef3 e3 f3 fs3 gf3 g3 gs3 af3 a3 as3 bf3 b3
      c4 cs4 df4 d4 ds4 ef4 e4 f4 fs4 gf4 g4 gs4 af4 a4 as4 bf4 b4
      c5 cs5 df5 d5 ds5 ef5 e5 f5 fs5 gf5 g5 gs5 af5 a5 as5 bf5 b5
      c6 cs6 df6 d6 ds6 ef6 e6 f6 fs6 gf6 g6 gs6 af6 a6 as6 bf6 b6
      c7 cs7 df7 d7 ds7 ef7 e7 f7 fs7 gf7 g7 gs7 af7 a7 as7 bf7 b7
      c8)
  (values 12 13 13 14 15 15 16 17 18 18 19 20 20 21 22 22 23
          24 25 25 26 27 27 28 29 30 30 31 32 32 33 34 34 35
          36 37 37 38 39 39 40 41 42 42 43 44 44 45 46 46 47
          48 49 49 50 51 51 52 53 54 54 55 56 56 57 58 58 59
          60 61 61 62 63 63 64 65 66 66 67 68 68 69 70 70 71
          72 73 73 74 75 75 76 77 78 78 79 80 80 81 82 82 83
          84 85 85 86 87 87 88 89 90 90 91 92 92 93 94 94 95
          96 97 97 98 99 99 100 101 102 102 103 104 104 105 106 106 107
          108))

;; The convention is that duration is always first, and the rest of the entity data is the tail or cdr
;; of the list

(define (mk-chord notes duration) (cons duration notes))
(define chord-duration car)
(define chord-notes cadr)

(define (mk-note note duration) (cons duration note))
(define note-duration car)
(define note-note cadr)

(define (mk-span start end data) (cons (cons start end) data))
(define span-of car)
(define span-start caar)
(define span-end cdar)
(define span-data cdr)
(define (span-length s)(- (span-end s) (span-start s)))

(define (gen-spans lst [spanf identity])

  ;; generates the list of spans of actual note data contained, filtering out the negative spans, but
  ;; reflecting the space they take up in the list. The spanf function must return the span-length of
  ;; each element of lst. This is usually the 'car' of the element, by local convention.
  ;;
  ;; The elements in the result list are ((span-start . span-end) . <original-lst-element>) for each
  ;; element in lst that is not a negative span.
  ;;

  (for/fold
   ([acc 0]
    [result '()] #:result (reverse result))
   ([l lst])
    (define s (spanf l))
    (cond
      [(< s 0)
       (define end (- acc s))
       (values end result)]
      [else
       (define end (+ acc s))
       (values end (cons (mk-span acc end l) result))])))

(define (index-spans spans posn)

  ;; find the span in the list of 'spans' the corresponds to posn. Uses a wrap-around (modulo)
  ;; function such that the list of spans can be considered as an infinitely repeating loop of spans

  (define pp (modulo posn (span-end (last spans))))
  (define (in? s v) (and (< v (span-end s)) (>= v (span-start s))))
  (index-of spans pp in?))

(define (project-chords rhythm chords selector [transpose identity])

  ;; given the list of rhythm lengths a note of the chords is 'projected' onto each element of the
  ;; rhythm, at the appropriate time position. When the rhythm is negative, it defines a rest in the
  ;; progression. The list of chords can be shorter than the rhythm; index-spans is used such that the
  ;; chord sequence repeats indefinitely.

  (define rhythm-spans (gen-spans rhythm))
  (define chord-spans (gen-spans chords chord-duration))
  (for/list ([p rhythm-spans])
    (define start (span-start p))
    (define data (transpose
                  (selector
                   (chord-notes (list-ref chords (index-spans chord-spans start))))))
    (list start (span-length p) data)))

(define (project-notes notes [transpose identity])

  ;; converts a list of notes (which may include rests) into absolute spans, ready for midi rendering.

  (define note-spans (gen-spans notes note-duration))
  (displayln note-spans)
  (for/list ([p note-spans])
    (define start (span-start p))
    (define data (cdr (span-data p)))
    (list start
          (span-length p)
          (if (number? data) (transpose data) data))))

(define join (compose flatten append))
(define (oct+ n) (+ n 12))
(define (oct++ n) (+ n 24))
(define (oct+++ n) (+ n 36))
(define (oct- n) (- n 12))
(define (oct-- n) (- n 24))
(define (oct--- n) (- n 36))

(define (mk-track name spans) (cons name spans))
(define track-name car)
(define track-spans cadr)

