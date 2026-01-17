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

(define (durations-of is) (map car is))
(define (notes-of is) (map cdr is))

(define (pad-right length . ds)
  (define pad (- length (foldl + 0 ds)))
  (append ds (list (- 0 pad) )))

(define mel-rhy-a1
  (list
   h dq e
   h dq e
   h dq e
   h hr

   h dq e
   q h er e
   h h
   h hr
   ))

(define pitches-a1
  (list

   e4 d4 c4
   g4 f4 e4
   a4 c5 a4
   g4

   f4 e4 d4
   f4 a4 e4
   f4 e4
   d4

   ))


(define bass-rhythm (repeat 8 w))
(define h-rhy-a bass-rhythm)

(define chords-a-spread
  (list
   ;; Spread chords
   (mk-chord w c3 g3 e4)  ;; C
   (mk-chord w e3 b3 g4)  ;; Em
   (mk-chord w f3 c3 a4)  ;; F
   (mk-chord w c3 g3 e4)  ;; C

   (mk-chord w f3 c3 a4)  ;; F
   (mk-chord w d3 a3 f4)  ;; Dm
   (mk-chord w f3 c3 a4)  ;; F
   (mk-chord w g3 d3 b3)  ;; G
   ))

(define chords-a-closed
  (list
   (mk-chord w c4 e4 g4)  ;; C
   (mk-chord w e4 g4 b4)  ;; Em
   (mk-chord w f4 a4 c5)  ;; F
   (mk-chord w c4 e4 g4)  ;; C

   (mk-chord w f4 a4 c5)  ;; F
   (mk-chord w d4 f4 a4)  ;; Dm
   (mk-chord w f4 a4 c5)  ;; F
   (mk-chord w g4 b4 d4)  ;; G
   ))


(define chords-a chords-a-closed)

(define bass-notes-a
  (list
   c3 e3 f3 c3

   f3 d3 f3 g3))

(define prj-zip (compose project-notes zip-notes))

(make-midi-track-file
 '(4 4)
 160     ;; 100bpm
 '(0 0) ;; c major
 "op45c.mid"
 (list

  (mk-track "Violins 1" (project-notes (zip-notes mel-rhy-a1 pitches-a1) va8))

  ;; (mk-track "Violins 2" (project-chords h-rhy-a chords-a second))
  ;; (mk-track "Violas" (project-chords h-rhy-a chords-a first))

  (mk-track "Trombone 1" (project-chords bass-rhythm chords-a third vb8))
  (mk-track "Trombone 2" (project-chords bass-rhythm chords-a first vb8))

  (mk-track "Cello" (prj-zip bass-rhythm bass-notes-a))
  (mk-track "Double bass" (prj-zip bass-rhythm (map vb8 bass-notes-a)))

  ;; (mk-track "Violins 2" (project-chords h-rhy-a chords-a third va8))
  ;; (mk-track "Violas" (project-chords h-rhy-a chords-a second))
  ;; (mk-track "Cello" (project-chords h-rhy-a chords-a first))
  ;; (mk-track "Double Bass" (project-chords h-rhy-a chords-a first vb8))

  ))

