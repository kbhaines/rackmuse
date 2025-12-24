#lang racket

(define q 10)
(define e (/ q 2))
(define er e)
(define bar (* 3 q))

; test piece is 4 bars
(define tlength (/ (* 4 bar) e))


; assuming smallest is eigth notes, for now

(define r1a (list q e er e e))
(define r1b (list e e e e e e))

(define chords (list (list '(b f c) bar) (list '(c e g) bar)))

(define chord-notes car)
(define chord-duration cadr)

(define (gen-spans lst spanf)
  (for/fold
   ([acc 0]
    [result '()] #:result result)
   ([l lst])
    (define end (+ acc (spanf l)))
    (values end (cons (cons (cons acc end) l) result))))

(define span-of car)
(define span-start caar)
(define span-end cdar)

(define (chord-at posn)
  (define spans (map span-end (gen-spans chords chord-duration)))
  (define pp (modulo posn (car spans)))
  ;; (displayln (~a posn spans pp))
  (list-ref chords (index-of (reverse spans) pp >)))

(for ([p (in-range 0 12)])
  (displayln (chord-at (* q p))))

(map span-of (reverse (gen-spans r1a identity)))

;; (chord-at 0)
;; (chord-at bar)
;; (chord-at (+ q bar))
;; (chord-at (* 2 bar))

