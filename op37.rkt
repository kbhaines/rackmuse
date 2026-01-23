#lang racket

(require
  "rackmuse.rkt"
  "instruments.rkt"
  "mid.rkt")


(define bar w)
(define barr (- 0 bar))
(define dbar (* 2 bar))
(define dbarr (- 0 dbar))

(define s (/ e 2))

(define bpm 80)
(define key-sig '(1 0) )
(define meter '(4 4))
(define filename-fmt "op37-~a.mid")

(define (generate-midi part-id tracks)
  (make-midi-track-file meter bpm key-sig (format filename-fmt part-id) tracks))


(define (dur d . args) (map (lambda(x)(cons d x)) args))

(define chords-a
  (list
   (major dbar g3)
   (inv (major dbar d3))
   (minor dbar e3)        ;; dissonance c/b with melody -8vb; sounds OK
   (minor bar a3)
   (major bar c4)

   ))

(define melody-a
  (list
   (cons qr 0) (cons q b4) (cons dq a4) (cons e g4) ; bar 1
   (cons dq d5) (cons e b4) (cons h a4) ; bar 2
   (cons qr 0) (cons q b4) (cons dq a4) (cons e b4) ; bar 3
   (cons dq fs5) (cons e g5) (cons q fs5) (cons q d5) ; bar 4

   (cons qr 0) (cons q d5) (cons q b4) (cons q g4) ; bar 5
   (cons h e5) (cons dq c5) (cons e b4) ; bar 6
   (cons h c5) (cons dq b4) (cons e a4) ; bar 7
   (cons h g4) (cons hr 0)  ; bar 8
   ))

(define inner-pulse-a (repeat 8 e er dh ))

(define tacet4 (cons (* 4 barr) 0))
(define tacet8 (cons (* 8 barr) 0))
(define tacet16 (cons (* 16 barr) 0))

(generate-midi
 'a
 (list

  (mk-track oboe-1 (project-notes melody-a))
  (mk-track cor-anglais (project-notes (cons tacet8 melody-a)))

  (mk-track horn-1 (project-notes (cons tacet8 melody-a) vb8))
  (mk-track trombone-1 (project-chords (durations-of (cons tacet8 chords-a)) chords-a third vb8 ))

  (mk-track violins-2 (project-chords (repeat 2 inner-pulse-a) chords-a third))
  (mk-track violas (project-chords (repeat 2 (durations-of chords-a)) chords-a second ))
  (mk-track cellos (project-chords (repeat 2 (durations-of chords-a)) chords-a first vb8))

  ))

;; Section B

(set! meter '(3 4))
(set! bar (* 3 q))

(define chords-b

  (list
   (mk-chord bar g2 d3 b3 a4)     ;; Gadd6
   (mk-chord q fs2 d3 a3 e4) (mk-chord h d2 a2 fs3)  ;; Dadd9/F#
   (mk-chord q e2 b2 g3 d3)  (mk-chord h e2 d3 g3 e3)  ;; Em7
   (mk-chord bar g2 c3 e3 c4)  ;; C/G

   (mk-chord bar g2 d3 b3 a4)     ;; Gadd6
   (mk-chord q fs2 d3 a3 e4) (mk-chord h d2 a2 fs3)  ;; Dadd9/F#
   (mk-chord q e2 b2 g3 d3)  (mk-chord h e2 d3 g3 e3)  ;; Em7
   (mk-chord bar g2 c3 e3 c4)  ;; C/G
   ))

(define melody-b1
  (zip-notes
   (list
    q q q
    h q
    dh
    h qr

    q q q
    h q
    h q
    h qr
    )
   (list

    g4 a4 d5
    e5 d5
    b4
    c5

    g4 a4 d5
    e5 d5 b4
    g4
    e4

    )))

(define viola-b-rhy (repeat 4 q q q q h  ))

(generate-midi
 'b
 (list

  (mk-track clarinet-1 (project-notes  melody-b1))
  (mk-track bassoon-1 (project-chords (durations-of chords-b) chords-b first va8 ))
  (mk-track violas (project-chords viola-b-rhy chords-b second va8 ))
  (mk-track cellos (project-chords (durations-of chords-b) chords-b first va8 ))

  ))

(generate-midi
 'b2
 (list

  (mk-track bassoon-1 (project-chords (durations-of chords-b) chords-b first va8 ))
  (mk-track violins-1 (project-chords (durations-of chords-b) chords-b third va16 ))
  (mk-track violins-2 (project-chords (durations-of chords-b) chords-b second va16 ))
  (mk-track violas (project-notes melody-b1 vb8))
  (mk-track cellos (project-chords (durations-of chords-b) chords-b first va8 ))
  (mk-track double-bass (project-chords viola-b-rhy chords-b first))

  ))

;; Section C

(set! meter '(4 4))
(set! bar (* 4 q))

(define section-c-harm-rhy (repeat 1 h h h dq e))
(define section-c-decor (repeat 1 e e dw qr))
;; (define section-c-bass-rhy (repeat 2 q (+ h e) e))
(define section-c-bass-rhy section-c-harm-rhy)

(define chords-c

  ;; chords-c is the same as chords-a but with the added fourth voice for the colour

  (list
   ;; The fourth note is the colour voice for horn
   (mk-chord dbar g3 b3 d4 a4)
   (mk-chord bar fs3 a3 d4 g4)
   (mk-chord bar fs3 a3 d4 a4)
   (mk-chord bar e3 g3 b3 b4)
   (mk-chord bar e3 g3 b3 e4)

   (mk-chord bar a3 c4 e4 a4)
   (mk-chord bar c4 e4 g4 e4)

   ;; repeat, but different cadence from Em
   (mk-chord dbar g3 b3 d4 a4)
   (mk-chord bar fs3 a3 d4 g4)
   (mk-chord bar d3 fs3 a3 a4)

   (mk-chord bar e3 a3 b3 fs4) ;; Es2-add4
   (mk-chord bar e3 g3 b3 e4)

   (mk-chord dbar a3 c4 e4 g4) ;; Am7
   (mk-chord dbar c4 d4 g4 e4) ;; Cs2

   (mk-chord bar g3 a3 d4 a4)  ;; Gs2
   (mk-chord bar g3 b3 d4 a4)  ;; G
   ))

(define counter-melody
  (zip-notes
   (list wr hr dq e h)
   (list d5 b4 a4)))

(define melody-coda

  ;; starts at bar12 of section

  (zip-notes
   (list
    q dq er e er
    q h e e
    dh qr
    q h e e
    dh qr
    dq e h
    wr
    w
    w
    )
   (list
    e5 fs5 d5
    fs5 g5 d5 b4
    d5
    c5 d5 a4 g4
    a4
    e4 g4 fs4
    c4
    a3
    )))

(define clen 10)

(define melc1 (append melody-a (take melody-a 11) melody-coda))

(generate-midi
 'c1
 (list

  (mk-track bassoon-1 (project-chords (repeat clen section-c-bass-rhy) chords-c first))

  (mk-track horn-1 (project-notes melc1 vb8))
  (mk-track horn-2 (project-chords (repeat clen section-c-harm-rhy) chords-c fourth))
  (mk-track trumpet-1 (project-notes (append counter-melody)))
  (mk-track trombone-1 (project-chords (repeat clen section-c-harm-rhy) chords-c third ))
  (mk-track trombone-2 (project-chords (repeat clen section-c-bass-rhy) chords-c first))

  (mk-track timpani (project-chords (repeat clen section-c-bass-rhy) chords-c first vb8))

  ;; Melody onto V1/V2 in octaves
  (mk-track violins-1 (project-notes melc1 va8))
  (mk-track violins-2 (project-notes melc1 ))

  ;; (mk-track "Melodyc" (project-notes melc va8))

  (mk-track violas (project-chords (repeat clen section-c-harm-rhy) chords-c fourth))
  (mk-track cellos (project-notes melc1 vb8))
  (mk-track double-bass (project-chords (repeat clen section-c-bass-rhy) chords-c first vb8))

  ))


