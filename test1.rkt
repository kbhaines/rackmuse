#lang racket

(require "rackmuse.rkt")

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

(define melody (list (cons dbarr 0) (cons dqr 0) (cons 0 "Main Melody") (cons e 60) (cons e 60) (cons e 60)
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
                       (mk-track "horns2" (project-notes melody oct+))
                       (mk-track "viola" (project-chords viola chords second oct+))
                       (mk-track "cello" (project-chords cello chords third))
                       (mk-track "dbass" (project-chords dbass chords first))))

