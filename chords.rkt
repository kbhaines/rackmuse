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

;; Duration is always first
(define chord-notes cadr)
(define chord-duration car)

(define note-note cadr)
(define note-duration car)

(define (gen-spans lst [spanf identity])

  ;; generates the list of spans of actual note data contained, filtering out the negative spans,
  ;; while reflecting the space they take up in the list. The spanf function must return the
  ;; span-length of an element of lst.
  ;;
  ;; The elements in the result list are ((span-start . span-end) . <original-lst-element>)
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
       (values end (cons (cons (cons acc end) l) result))])))

(define span-of car)
(define span-start caar)
(define span-end cdar)
(define span-data cdr)
(define (span-length s)(- (span-end s) (span-start s)))

(define join (compose flatten append))

(define r1a (list q e er e e))
(define r1b (list e e e e e e))
(define r1 (join r1a r1b))
(define r2 (list e er er dqr dqr qr e))

(define intro-chords (list (list dbar '(46 48 53)) (list dbar '(48 52 55))))

;; put some tests here
;; (define r1a-spans (gen-spans r1a))
;; (define r1b-spans (gen-spans r1b))

;; (pretty-print (gen-spans (join r2 r2 r2 r2) identity))
;; (exit 0)

(define tacet1 (list dbar))
(define tacet2 (list dbarr))
(define tacet4 (list dbarr dbarr))

(define dbass (join r2 r2 r2 r2))
(define cello (join r1 r1 r1 r1))
(define viola (join tacet4 r1 r1 r1 r1))
(define chords (append intro-chords intro-chords))

(define melody (list (cons dbarr 0) (cons dqr 0) (cons e 60) (cons e 60) (cons e 60)
                     (cons q 65)))

(define (index-spans spans posn)
  (define pp (modulo posn (span-end (last spans))))
  (define (in? s v) (and (< v (span-end s)) (>= v (span-start s))))
  (index-of spans pp in?))

(define (project-chords rhythm chords [selector identity])
  (define rhythm-spans (gen-spans rhythm))
  (define chord-spans (gen-spans chords chord-duration))
  (for/list ([p rhythm-spans])
    (define start (span-start p))
    (list start
          (span-length p)
          (selector (chord-notes (list-ref chords (index-spans chord-spans start)))))))

(define (project-notes notes)
  (define note-spans (gen-spans notes note-duration))
  (displayln note-spans)
  (for/list ([p note-spans])
    (define start (span-start p))
    (list start
          (span-length p)
          (cdr (span-data p)))))

(require "mid.rkt")

(pretty-display (project-notes melody))
(pretty-display (project-chords dbass chords first))
(make-midi-track-file "out.mid"
                      (list
                       (project-notes melody)
                       (project-chords viola chords second)
                       (project-chords cello chords third)
                       (project-chords dbass chords first)))
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
