#lang racket

(define q 10)
(define qr (- 0 q))
(define dqr (- 0 (* 3 (/ q 2))))
(define e (/ q 2))
(define er (- 0 e))

(define bar (* 3 q))
(define dbar (* 2 bar))

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

(define chord-notes cadr)
(define chord-duration car)

(define (gen-spans lst [spanf identity])
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
(define (span-length s)(- (span-end s) (span-start s)))

(define join (compose flatten append))

(define r1a (list q e er e e))
(define r1b (list e e e e e e))
(define r1 (join r1a r1b))
(define r2 (list e er er dqr dqr qr e))

(define intro-chords (list (list dbar '(b f c)) (list dbar '(c e g))))

(define r1a-spans (gen-spans r1a))
(define r1b-spans (gen-spans r1b))

(define (index-spans spans posn)
  (define pp (modulo posn (span-end (last spans))))
  (define (in? s v) (and (< v (span-end s)) (>= v (span-start s))))
  (index-of spans pp in?))

;; (pretty-print (gen-spans (join r2 r2 r2 r2) identity))
;; (exit 0)

(define dbass (join r2 r2 r2 r2))
(define cello (join r1 r1 r1 r1))
(define chords (append intro-chords intro-chords))

(define (project-chords rhythm chords [selector identity])
  (define rhythm-spans (gen-spans rhythm))
  (define chord-spans (gen-spans chords chord-duration))
  (for/list ([p rhythm-spans])
    (define start (span-start p))
    (list start
          (span-length p)
          (selector (chord-notes (list-ref chords (index-spans chord-spans start)))))))

(pretty-display (project-chords dbass chords first))
(pretty-display (project-chords cello chords second))

(exit 0)

(map span-of r1a-spans)
(for ([p (in-range 0 12)])
  (displayln (index-spans r1a-spans (* e p))))
(map span-of r1b-spans)

;; (define (chord-at posn)
;;   (define spans (map span-end (gen-spans chords chord-duration)))
;;   (define pp (modulo posn (last spans)))
;;   ;; (displayln (~a posn spans pp))
;;   (list-ref chords (index-of spans pp >)))

;; (for ([p (in-range 0 12)])
;;   (displayln (index-spans chord-spans (* q p)))
;;   (displayln (chord-at (* q p))))
