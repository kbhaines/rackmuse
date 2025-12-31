#lang racket

(require "rackmuse.rkt")

(define r1a (list q e er e e))
(define r1b (list e e e e e e))
(define r1 (join r1a r1b))
(define r2 (list e er er dqr dqr qr e))

(define bar (* 6 e))
(define barr (- 0 bar))
(define dbar (* 2 bar))
(define dbarr (- 0 dbar))

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

(define melody-b
  (list
   (cons dq bf4) (cons er 0) (cons e bf4) (cons e c5)
   (cons h bf4) (cons er 0) (cons e g4)
   (cons dq ef5) (cons dq d5)
   (cons h c5) (cons q bf4)
   (cons (+ dh h) f4) (cons er 0) (cons e a4)
   (cons dh bf4) (cons er 0)
   ))

(define chords-b
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
  (repeat 4
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
  (mk-track "Trumpet 1" (project-notes melody-b))
  (mk-track "Horn 1" (project-notes melody-b oct-))
  (mk-track "Horn 2" (project-notes melody-b oct-))
  (mk-track "Trombone 1" (project-chords trombones-b chords-b second oct+))
  (mk-track "Trombone 2" (project-chords trombones-b chords-b third))
  (mk-track "Trombone 3" (project-chords trombones-b chords-b first))
  (mk-track "Violins 2" (project-chords violins-b chords-b third oct+))
  (mk-track "Violas" (project-chords viola-b chords-b second oct+))
  (mk-track "Cellos" (project-chords cello-b chords-b first))
  (mk-track "Double Bass" (project-chords dbass-b chords-b first oct-))))

