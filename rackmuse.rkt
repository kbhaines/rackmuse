#lang racket

(provide
 PPQ
 w wr dw dwr
 h hr dh dhr
 q qr dq dqr
 e er de der
 s sr ds dsr

 repeat rest dot
 tacet

 mk-chord chord-notes chord-duration inv
 major minor
 mk-note note-note note-duration

 ;; project-chords project-notes
 arpeg
 pitch-scale-degrees
 zip-notes
 transpose

 timeline-length timeline-index timeline-ref
 durations->timeline
 harmony->voice

 mk-track track-name track-spans

 join

 va8 va16 va24 vb8 vb16 vb24

 c0 cs0 df0 d0 ds0 ef0 e0 f0 fs0 gf0 g0 gs0 af0 a0 as0 bf0 b0
 c1 cs1 df1 d1 ds1 ef1 e1 f1 fs1 gf1 g1 gs1 af1 a1 as1 bf1 b1
 c2 cs2 df2 d2 ds2 ef2 e2 f2 fs2 gf2 g2 gs2 af2 a2 as2 bf2 b2
 c3 cs3 df3 d3 ds3 ef3 e3 f3 fs3 gf3 g3 gs3 af3 a3 as3 bf3 b3
 c4 cs4 df4 d4 ds4 ef4 e4 f4 fs4 gf4 g4 gs4 af4 a4 as4 bf4 b4
 c5 cs5 df5 d5 ds5 ef5 e5 f5 fs5 gf5 g5 gs5 af5 a5 as5 bf5 b5
 c6 cs6 df6 d6 ds6 ef6 e6 f6 fs6 gf6 g6 gs6 af6 a6 as6 bf6 b6
 c7 cs7 df7 d7 ds7 ef7 e7 f7 fs7 gf7 g7 gs7 af7 a7 as7 bf7 b7
 c8

 major-scale
 durations-of
 notes-of

 blueprint->midi blueprint->orchestrated-midi
 (struct-out assign)
 )

(require racket/generator)

(define PPQ 960)

(define (rest d) (- 0 d))
(define (dot d) (* 3 (/ d 2)))

(define w (* 4 PPQ))
(define wr (rest w))
(define dw (dot w))
(define dwr (rest dw))

(define h (* 2 PPQ))
(define hr (rest h))
(define dh (dot h))
(define dhr (rest dh))

(define q PPQ)
(define qr (rest q))
(define dq (dot q))
(define dqr (rest dq))

(define e (/ q 2))
(define er (rest e))
(define de (dot e))
(define der (rest de))

(define s (/ q 4))
(define sr (rest s))
(define ds (dot s))
(define dsr (rest ds))

(define (repeat n . ls) (flatten (make-list n (list ls))))

(define (tacet d n) (cons (* (- 0 d) n) 0))

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

;; NOTE: The convention for the user facing data (e.g. chords, notes) is that duration is always
;; first, and the rest of the entity data is the tail or cdr of the list. This will hold true most of
;; the time for user-facing data; but sometimes, for convenience, the *internals* of the module are
;; allowed to violate it where it makes sense.

(define (durations-of is) (map car is))
(define (notes-of is) (map cdr is))

(define (mk-chord duration . notes) (list duration notes))
(define chord-duration car)
(define chord-notes cadr)

(define (major duration root) (mk-chord duration root (+ root 4) (+ root 7)))
(define (minor duration root) (mk-chord duration root (+ root 3) (+ root 7)))

(define (inv chord [degree 1])
  (define-values (p1 p2) (split-at (chord-notes chord) degree))
  (apply mk-chord (chord-duration chord) (append p2 (map va8 p1))))

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
      [(and (number? s) (< s 0))
       (define end (- acc s))
       (values end result)]
      [(and (number? s) (> s 0))
       (define end (+ acc s))
       (values end (cons (mk-span acc end l) result))]
      [else (values acc result)])))

(define (index-spans spans posn)

  ;; find the span in the list of 'spans' the corresponds to posn. Uses a wrap-around (modulo)
  ;; function such that the list of spans can be considered as an infinitely repeating loop of spans

  (define pp (modulo posn (span-end (last spans))))
  (define (in? s v) (and (< v (span-end s)) (>= v (span-start s))))
  (index-of spans pp in?))

(define (project-chords rhythm chords selector [xform identity])

  ;; DEPRECATED - use harmony->voice
  ;; given the list of rhythm lengths a note of the chords is 'projected' onto each element of the
  ;; rhythm, at the appropriate time position. When the rhythm is negative, it defines a rest in the
  ;; progression. The list of chords can be shorter than the rhythm; index-spans is used such that the
  ;; chord sequence repeats indefinitely.
  ;;
  ;;

  (define rhythm-spans (gen-spans rhythm))
  (define chord-spans (gen-spans chords chord-duration))
  (for/list ([p rhythm-spans])
    (define start (span-start p))
    (define data (xform
                  (selector
                   (chord-notes (list-ref chords (index-spans chord-spans start))))))
    (list start (span-length p) data)))

(define chords->notes project-chords)

(define (durations->timeline dur)
  (for/fold
   ([td 0]
    [result '()] #:result (reverse result))
   ([d dur])
    (values (+ td (abs d)) (cons (cons td d) result))))

(define (timeline-ref tl t [infinite? #f])
  (define tgt (if infinite? (modulo t (timeline-length tl)) t))
  (define (found? p) (<= (car p) tgt (+ (car p) (- (abs (cdr p)) 1))))
  (findf found? tl))

(define (timeline-index tl t [infinite? #f])
  (define tgt (if infinite? (modulo t (timeline-length tl)) t))
  (define (found? p) (<= (car p) tgt (+ (car p) (- (abs (cdr p)) 1))))
  (index-where tl found?))

(define (timeline-length tl)
  (define p (last tl))
  (+ (car p) (abs (cdr p))))

(define (harmony->voice rhy chords selector [xform identity])
  (define rhy-timeline (durations->timeline rhy))
  (define chds-timeline (durations->timeline (durations-of chords)))

  (define select
    (cond
      [(not selector) (lambda(_time _dur v) v)]
      [(eq? 1 (procedure-arity selector)) (lambda(_time _dur v)(selector v))]
      [else selector]))

  (define get-notes (if (not selector) cdr chord-notes))
  (for/list ([r rhy-timeline])
    (define time (car r))
    (define dur (cdr r))
    (if (> dur 0)
        (cons dur (xform
                   (select time dur
                           (get-notes
                            (list-ref chords (timeline-index chds-timeline time #t))))))
        (cons dur 0))))

(define (notes->spans notes [xform identity])

  ;; converts a list of notes (which may include rests) into absolute spans, ready for midi rendering.

  (define note-spans (gen-spans notes note-duration))
  (for/list ([p note-spans])
    (define start (span-start p))
    (define data (cdr (span-data p)))
    (list start
          (span-length p)
          (if (number? data) (xform data) data))))

(define (arpeg pitch-indices)

  ;; Creates a selector function to pass to harmony->voice The selector cycles through the given
  ;; pitch-indices every time harmony->voice 'needs' a note.
  ;;
  ;; Example:
  ;;    (harmony->voice (list q q qr qr q q) chords-c (arpeg '(0 2 3 0)))

  (define igen
    (infinite-generator
     (for ([p pitch-indices])
       (yield p))))
  (lambda(lst)
    (define next (igen))
    (cond
      [(pair? next)
       (define idx (car next))
       (define other (cdr next))
       (other (list-ref lst idx))]
      [else (list-ref lst next)])))

(define (pitch-scale-degrees scale root pitches)
  (for/list ([p pitches])
    (define pp (modulo (- p root) 12))
    (index-of scale pp)))

(define (zip-notes durations pitches)

  ;; combines the list of durations and pitches together, creating a list of (duration . pitch) pairs.
  ;; Rests are not assigned to pitches (obviously), but are left in correct position in the resulting
  ;; list.

  (for/fold
   ([ps pitches]
    [ns '()] #:result (reverse ns))
   ([d durations])
    (if (and (number? d) (> d 0) ps)
        (values (cdr ps) (cons (mk-note (car ps) d) ns))
        (values ps (cons (mk-note 0 d) ns)))))

(define (transpose f notes)

  ;; applies the given transposition function to all the notes, returning
  ;; a new list of notes

  (for/list ([n notes])
    (define dur (car n))
    (define pitch (cdr n))
    (cons dur (f pitch))))

(define major-scale '(0 2 4 5 7 9 11))
(define join (compose flatten append))

(define (va8 n) (+ n 12))
(define (va16 n) (+ n 24))
(define (va24 n) (+ n 36))
(define (vb8 n) (- n 12))
(define (vb16 n) (- n 24))
(define (vb24 n) (- n 36))

(define (mk-track name notes) (cons name (notes->spans notes)))
(define track-name car)
(define track-spans cadr)

(struct assign (src dst xform) #:transparent)

(define (blueprint->midi bluep)
  (hash-map bluep (lambda (tid notes) (mk-track (format "~a" tid) notes)) #t))

(define (blueprint->orchestrated-midi bluep orch-assigns)
  (for/list ([oa orch-assigns])
    (define src-data (hash-ref bluep (assign-src oa)))
    (mk-track
     (assign-dst oa)
     (if (procedure? (assign-xform oa)) (transpose (assign-xform oa) src-data) src-data))))

