#lang racket

(require
  "rackmuse.rkt"
  "mid.rkt")


(define bar w)
(define barr (- 0 bar))
(define dbar (* 2 bar))
(define dbarr (- 0 dbar))

(define s (/ e 2))

(define (dur d . args) (map (lambda(x)(cons d x)) args))

(define melody-a1
  (list
   (cons q c5) (cons dq f5) (cons er 0) (cons e bf5) (cons s a5) (cons s g5)
   (cons q f5) (cons dq bf4) (cons dqr 0)

   (cons q f5) (cons dq bf5) (cons er 0) (cons e c6) (cons s bf5) (cons s a5)
   (cons e bf5) (cons e d6) (cons dq bf5) (cons dqr 0)

   ))

(define melody-a2
  (append
   (dur e f6 ef6 d6 c6 d6 c6 bf5 c6)
   (list (cons h g5) (cons q bf5) (cons qr 0))
   (dur e c6 bf5 c6 bf5 a5 bf5 a5 g5)
   (list (cons dq f5) (cons dq bf5) (cons q a5)
         (cons h bf5))
   ))


(define (durations-of is) (map car is))
(define (notes-of is) (map cdr is))

(define melody-rhy (durations-of melody-a1))

(define melody-a2-notes (list f5 bf5 0 c6 bf5 a5 bf5 d6 bf5))

;; (displayln melody-rhy)
;; (displayln (notes-of melody-a))
;; (displayln melody-a2-notes)
;; (exit 1)

(define chords-a1
  (list
   (mk-chord h bf2 f3 c4) (mk-chord q c3 f3 c4)  (mk-chord q d3 f3 c4)
   (mk-chord w ef3 f3 c4)
   (mk-chord h bf2 f3 c4) (mk-chord q c3 f3 c4)  (mk-chord q d3 f3 c4)
   (mk-chord w ef3 f3 c4)
   ))


(define chords-a2
  (list
   (mk-chord h g3 bf3 d4) (mk-chord q g3 c4 f4) (mk-chord q f3 c4 f4)
   (mk-chord h ef3 bf3 f4) (mk-chord h ef3 bf3 f4)
   (mk-chord h g3 bf3 d4) (mk-chord q g3 c4 f4) (mk-chord q f3 c4 f4)
   (mk-chord h ef3 bf3 f4) (mk-chord h ef3 bf3 f4)

   (mk-chord w bf2 f3 c4)))

(dur e c5 c6 c7)

(define rhy-a1
  (repeat 16 s s e q))

(define harm-a (durations-of chords-a1))

(define harm-rhy (durations-of (append chords-a1 chords-a2)))

(make-midi-track-file
 '(4 4)
 '(-2 0) ;; bflat
 "op45.mid"
 (list

  ;; (mk-track "Oboe1" (project-chords trombones-b b-chords third oct+++))
  ;; (mk-track "Oboe2" (project-chords trombones-b b-chords first oct+++))

  (mk-track "Oboe 1:melody" (project-notes (append melody-a1 melody-a2)))
  (mk-track "Violins 1" (project-chords rhy-a1 (append chords-a1 chords-a2) second va8))
  (mk-track "Violins 2" (project-chords harm-rhy (append chords-a1 chords-a2) third va8))
  (mk-track "Cello" (project-chords rhy-a1 (append chords-a1 chords-a2) first))

  (mk-track "Horn 1" (project-chords harm-rhy (append chords-a1 chords-a2) third))
  (mk-track "Horn 2" (project-chords harm-rhy (append chords-a1 chords-a2) second va8))
  (mk-track "Trombone 1" (project-chords harm-rhy (append chords-a1 chords-a2) first))
  ))

