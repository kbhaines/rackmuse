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

(define chords-a
  (list
   (major dbar g3)
   (inv (major dbar d3))
   (minor dbar e3)        ;; dissonance c/b with melody -8vb; sounds OK
   (inv (minor bar a3) 0)
   (inv (major bar c4) 0)

   ))

(define melody-a
  (list
   (cons qr 0) (cons q b4) (cons q a4) (cons q g4) ; bar 1
   (cons dq d5) (cons e b4) (cons h a4) ; bar 2
   (cons qr 0) (cons q b4) (cons q a4) (cons q b4) ; bar 3
   (cons dq fs5) (cons e g5) (cons q fs5) (cons q d5) ; bar 4
   (cons qr 0) (cons q d5) (cons q b4) (cons q g4) ; bar 5
   (cons h e5) (cons dq c5) (cons e b4) ; bar 6
   (cons h c5) (cons dq b4) (cons e a4) ; bar 7
   (cons h g4) (cons hr 0)  ; bar 8
   ))

(define inner-pulse-a (repeat 8 e er dh ))

(make-midi-track-file
 '(4 4)
 80
 '(1 0) ;; Gmajor
 "op37.mid"
 (list

  ;; (mk-track "Oboe 1:melody" (project-notes bass-notes-b))


  ;; (mk-track "Melody" (project-notes (zip-notes (repeat 12 bar) mel-skel-b)))

  (mk-track "Cor Anglais" (project-notes melody-a))

  (mk-track "Violins 2" (project-chords inner-pulse-a chords-a third  ))
  (mk-track "Violas" (project-chords (durations-of chords-a) chords-a second ))
  (mk-track "Cello" (project-chords (durations-of chords-a) chords-a first vb8))

  (mk-track "Horn 1" (project-notes melody-a vb8))
  (mk-track "Trombone 1" (project-chords (durations-of chords-a) chords-a third vb8 ))

  ))

