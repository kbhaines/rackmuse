#lang racket

(require "rackmuse.rkt")

(define r1a (list q e er e e))
(define r1b (list e e e e e e))
(define r1 (join r1a r1b))
(define r2 (list e er er dqr dqr qr e))

(define intro-chords (list (list dbar '(46 48 53)) (list dbar '(48 52 55))))

(define a-chords (list (list dbar (list bf2 c3 f3))))

;; put some tests here
;; (define r1a-spans (gen-spans r1a))
;; (define r1b-spans (gen-spans r1b))

(define dbass (join r2 r2 r2 r2))
(define cello (join r1a r1a r1a r1a))
(define viola (join r1a r1a r1a r1a))
(define chords (append a-chords a-chords))

(define melody
  (list (cons dqr 0) (cons qr 0)
        (cons 0 "Main Melody")
        (cons e c4)
        (cons (+ dq e) c4) (cons er 0) (cons e f4)
        (cons (+ dq e) f4) (cons er 0) (cons e bf4)
        (cons dq bf4) (cons dq a4)
        ))

(require "mid.rkt")
(displayln (project-notes melody))

;; (pretty-display (project-notes melody))
;; (pretty-display (project-chords dbass chords first))
(make-midi-track-file '(6 8)
                      '(-2 0)
                      "out.mid"
                      (list
                       (mk-track "horns" (project-notes melody))
                       (mk-track "viola" (project-chords viola chords third))
                       (mk-track "cello" (project-chords cello chords first))
                       (mk-track "dbass" (project-chords dbass chords first oct-))))

