#lang racket

(require "rackmuse.rkt")

(define r1a (list q e er e e))
(define r1b (list e e e e e e))
(define r1 (join r1a r1b))
(define r2 (list e er er dqr dqr qr e))

(define a-chords (list (list dbar (list bf2 c3 f3))))

;; put some tests here
;; (define r1a-spans (gen-spans r1a))
;; (define r1b-spans (gen-spans r1b))

(define dbass (join r2 r2 r2 r2))
(define cello (join r1a r1a r1a r1a r1a r1a r1a r1a))
(define viola (join r1 r1 r1 r1))
(define chords (append a-chords a-chords))

(define melody
  (list (cons barr 0)
        (cons dqr 0) (cons qr 0)
        (cons 0 "Main Melody")
        (cons e c4)
        (cons (+ dq e) c4) (cons er 0) (cons e f4)
        (cons (+ dq e) f4) (cons er 0) (cons e bf4)
        (cons dq bf4) (cons dq a4)
        (cons dq g4) (cons dq f4)
        (cons dh c4) (cons barr 0)
        ))

(require "mid.rkt")
(displayln (project-notes melody))

;; (pretty-display (project-notes melody))
;; (pretty-display (project-chords dbass chords first))
(make-midi-track-file '(6 8)
                      '(-2 0)
                      "intro.mid"
                      (list
                       (mk-track "horns" (project-notes melody))
                       (mk-track "viola" (project-chords viola chords third))
                       (mk-track "cello" (project-chords cello chords first))
                       (mk-track "dbass" (project-chords dbass chords first oct-))))

(define bars4 (* 2 dbar))

(define b-chords
  (list
   (list bars4 (list g2 bf2 d3)) ;; Gm
   (list dbar (list g2 bf2 ef3)) ;; Eb/G
   (list dbar (list f2 bf2 d3)) ;; Bb/F
   (list bars4 (list g2 bf2 d3)) ;; Gm
   (list dbar (list g2 bf2 ef3)) ;; Eb/G
   (list dbar (list f2 a2 c3)) ;; F
   ))

(define violins-b
  (list
   e er e er e e
   e er e er e e
   e er e er e e
   e er e er e e
   q e er e e
   q e er e e
   q e er e e
   q e er e e))

(define viola-b violins-b)
(define cello-b violins-b)

(define dbass-b
  (repeat 8
          e qr e qr
          e qr qr e
          ))

(define trombones-b
  (append
   (repeat 4 er e e e qr)
   (repeat 2 q e er e e)
   (list (+ dq q) er)
   (list e e e e e e)))

(make-midi-track-file
 '(6 8)
 '(-2 0)
 "b-section.mid"
 (list
  ;; (mk-track "Oboe1" (project-chords trombones-b b-chords third oct+++))
  ;; (mk-track "Oboe2" (project-chords trombones-b b-chords first oct+++))
  (mk-track "Trombone1" (project-chords trombones-b b-chords second oct+))
  (mk-track "Trombone2" (project-chords trombones-b b-chords third))
  (mk-track "Trombone3" (project-chords trombones-b b-chords first))
  (mk-track "Violins-II" (project-chords violins-b b-chords third oct+))
  (mk-track "Viola" (project-chords viola-b b-chords second oct+))
  (mk-track "Cello" (project-chords cello-b b-chords first))
  (mk-track "Dbass" (project-chords dbass-b b-chords first oct-))))

