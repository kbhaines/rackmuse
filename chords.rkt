#lang racket

(define PPQ 480)
(define q PPQ)
(define qr (- 0 q))
(define dqr (- 0 (* 3 (/ q 2))))
(define e (/ q 2))
(define er (- 0 e))

(define bar (* 3 q))
(define barr (- 0 bar))
(define dbar (* 2 bar))
(define dbarr (- 0 dbar))

#;(define-values
    (
     c0
     c#0
     db0
     d0
     d#0
     eb0
     e0
     f0
     f#0
     gb0
     g0
     g#0
     ab0
     a0
     a#0
     bb0
     b0
     )
    0)

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

(define (project-chords rhythm chords [selector identity])

  ;; given the list of rhythm spans, a note of the chords is 'projected' onto each element of the
  ;; rhythm, at the appropriate time position. The list of chords can be shorter than the rhythm;
  ;; index-spans is used such that the chord sequence repeats indefinitely.

  (define rhythm-spans (gen-spans rhythm))
  (define chord-spans (gen-spans chords chord-duration))
  (for/list ([p rhythm-spans])
    (define start (span-start p))
    (list start
          (span-length p)
          (selector (chord-notes (list-ref chords (index-spans chord-spans start)))))))

(define (project-notes notes)

  ;; converts a list of notes (which may include rests) into absolute spans, ready for midi rendering.

  (define note-spans (gen-spans notes note-duration))
  (displayln note-spans)
  (for/list ([p note-spans])
    (define start (span-start p))
    (list start
          (span-length p)
          (cdr (span-data p)))))

(define join (compose flatten append))

(define (mk-track name spans) (cons name spans))
(define track-name car)
(define track-spans cadr)

;; Here comes the tune...

(define r1a (list q e er e e))
(define r1b (list e e e e e e))
(define r1 (join r1a r1b))
(define r2 (list e er er dqr dqr qr e))

(define intro-chords (list (list dbar '(46 48 53)) (list dbar '(48 52 55))))

;; put some tests here
;; (define r1a-spans (gen-spans r1a))
;; (define r1b-spans (gen-spans r1b))


(define tacet1 (list dbar))
(define tacet2 (list dbarr))
(define tacet4 (list dbarr dbarr))

(define dbass (join r2 r2 r2 r2))
(define cello (join r1 r1 r1 r1))
(define viola (join tacet4 r1 r1 r1 r1))
(define chords (append intro-chords intro-chords))

(define melody (list (cons dbarr 0) (cons dqr 0) (cons 0 "hello") (cons e 60) (cons e 60) (cons e 60)
                     (cons q 65)))

(require "mid.rkt")
(displayln (project-notes melody))

;; (pretty-display (project-notes melody))
;; (pretty-display (project-chords dbass chords first))
(make-midi-track-file '(6 8)
                      '(-2 0)
                      "out.mid"
                      (list
                       (mk-track "horns" (project-notes melody))
                       (mk-track "viola" (project-chords viola chords second))
                       (mk-track "cello" (project-chords cello chords third))
                       (mk-track "dbass" (project-chords dbass chords first))))

;; (pretty-display (project-chords cello chords second))
;; (pretty-display (project-chords viola chords third))

(exit 0)

;; (map span-of r1a-spans)
;; (for ([p (in-range 0 12)])
;;   (displayln (index-spans r1a-spans (* e p))))
;; (map span-of r1b-spans)

;; (define (chord-at posn)
;;   (define spans (map span-end (gen-spans chords chord-duration)))
;;   (define pp (modulo posn (last spans)))
;;   ;; (displayln (~a posn spans pp))
;;   (list-ref chords (index-of spans pp >)))

;; (for ([p (in-range 0 12)])
;;   (displayln (index-spans chord-spans (* q p)))
;;   (displayln (chord-at (* q p))))
