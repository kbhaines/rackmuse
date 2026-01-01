#lang racket

(require
  "rackmuse.rkt"
  "mid.rkt")

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

(displayln (project-notes melody))

(make-midi-track-file '(6 8)
                      '(-2 0)
                      "intro.mid"
                      (list
                       (mk-track "horns" (project-notes melody))
                       (mk-track "viola" (project-chords viola chords third))
                       (mk-track "cello" (project-chords cello chords first))
                       (mk-track "dbass" (project-chords dbass chords first vb8))))

(define bars4 (* 2 dbar))

(define melody-b
  (list
   (cons 0 "Melody B")
   (cons dq bf4) (cons er 0) (cons e bf4) (cons e c5)
   (cons h bf4) (cons er 0) (cons e g4)
   (cons dq ef5) (cons dq d5)
   (cons h c5) (cons q bf4)
   (cons (+ dh h) f4) (cons er 0) (cons e a4)
   (cons dh bf4) (cons er 0)
   ))

(define chords-b
  (list
   (list dbar (list g2 bf2 d3)) ;; Gm
   (list dq (list g2 bf2 ef3)) ;; Eb/G
   (list dq (list g2 bf2 d3)) ;; Gm
   (list bar (list g2 c2 d3)) ;; Gs4
   (list dbar (list g2 bf2 ef3)) ;; Eb/G
   (list dbar (list f2 bf2 d3)) ;; Bb/F

   (list bars4 (list g2 bf2 d3)) ;; Gm
   (list dbar (list g2 bf2 ef3)) ;; Eb/G
   (list dbar (list f2 a2 c3)) ;; F
   ))

(define pattern-b
  (list
   e er e er e e
   e er e er e e
   e er e er e e
   e er e er e e
   q e er e e
   q e er e e
   q e er e e
   q e er e e))

(define violins-b pattern-b)
(define viola-b pattern-b)
(define cello-b pattern-b)

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

(define (horn-melody-b p) (if (eq? p f4) f3 (vb8 p)))

(make-midi-track-file
 '(6 8)
 '(-2 0)
 "b-section.mid"
 (list
  ;; (mk-track "Oboe1" (project-chords trombones-b b-chords third oct+++))
  ;; (mk-track "Oboe2" (project-chords trombones-b b-chords first oct+++))
  (mk-track "Trumpet 1:melody" (project-notes melody-b))

  (mk-track "Horn 1:melody" (project-notes melody-b horn-melody-b))
  (mk-track "Horn 2:melody" (project-notes melody-b horn-melody-b))

  (mk-track "Trombone 1:engine" (project-chords trombones-b chords-b second va8))
  (mk-track "Trombone 2:engine" (project-chords trombones-b chords-b third))
  (mk-track "Trombone 3:engine" (project-chords trombones-b chords-b first))

  (mk-track "Violins 2:engine" (project-chords violins-b chords-b third va8))
  (mk-track "Violas:engine" (project-chords viola-b chords-b second va8))
  (mk-track "Cellos:bass" (project-chords cello-b chords-b first))
  (mk-track "Double Bass:bass" (project-chords dbass-b chords-b first vb8))))

