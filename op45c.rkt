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

(define mel-rhy-a
  (list
   q q q er e
   q dq qr e
   q q q er e
   dq e q qr
   q
   ))

(define pitches-a
  (list
   f4 bf4 c5 bf4
   c5 d5 bf4
   c5 d5 ef5 d5
   c5 bf4 a4))

(define (pad-right length . ds)
  (define pad (- length (foldl + 0 ds)))
  (append ds (list (- 0 pad) )))

;; (displayln (pad-right bar q q q))
;; (exit 1)

(define mel-rhy-a1
  (list
    h qr e e
    h qr e e
    h q q
    h hr

    h qr e e
    h qr e e
    h q er e
    h hr

   ))

(define pitches-a1
  (list

    e4 f4 e4
    g4 g4 f4
    a4 c5 a4
    g4

    f4 e4 d4 
    f4 e4 d4
    g4 f4 e4
    d4

    e4 f4 e4
    g4 g4 f4
    a4 c5
    g4

    f4 f4 e4 
    g4 b4 a4
    c5 d5 c5
    b4

   ;; cs4 ds4 ds4
   ;; e4 fs4 gs4 b3 ;;cs4
   ;; cs4 ds4 ds4 e4
   ;; fs4 gs4 as4
   ;;
   ;; b4 as4 b4 ds4
   ;; e4
   ))

(define bass-notes-a
  (list
    c3
    c3
    f3
    c3
    d3
    d3
    f3
    g3))


(define bass-rhythm 
  (repeat 8 q qr q qr))

(make-midi-track-file
 '(4 4)
 100     ;; 80bpm
 '(-2 0) ;; bflat
 "op45c.mid"
 (list

  (mk-track "melody1" (project-notes (zip-notes mel-rhy-a1 pitches-a1)))
  ;; (mk-track "bass1" (project-chords bass-rhythm bass-notes-a first))

  ;; (mk-track "Violins 1" (project-notes (zip-notes mel-rhy-a1 pitches-a1) vb8))
  ;; (mk-track "Violins 2" (project-chords h-rhy-a chords-a third va8))
  ;; (mk-track "Violas" (project-chords h-rhy-a chords-a second))
  ;; (mk-track "Cello" (project-chords h-rhy-a chords-a first))
  ;; (mk-track "Double Bass" (project-chords h-rhy-a chords-a first vb8))

  ))

