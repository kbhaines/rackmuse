#lang racket

(require
  "rackmuse.rkt"
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

   ;; (mk-chord bar a3 a3 c4)
   ;; (mk-chord bar c4 c4 e4)
   (minor bar a3)
   (major bar c4)

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

(define tacet4 (cons (* 4 barr) 0))
(define tacet8 (cons (* 8 barr) 0))

(generate-midi
 'a
 (list

  (mk-track "Oboe" (project-notes melody-a))
  (mk-track "Cor Anglais" (project-notes (cons tacet8 melody-a)))

  (mk-track "Horn 1" (project-notes (cons tacet8 melody-a) vb8))
  (mk-track "Trombone 1" (project-chords (durations-of (cons tacet8 chords-a)) chords-a third vb8 ))

  (mk-track "Violins 2" (project-chords (repeat 2 inner-pulse-a) chords-a third))
  (mk-track "Violas" (project-chords (repeat 2 (durations-of chords-a)) chords-a second ))
  (mk-track "Cello" (project-chords (repeat 2 (durations-of chords-a)) chords-a first vb8))

  ))

(define chords-b
  (list
   (mk-chord q g2 d3 a3 d4)     ;; Gadd6
   (mk-chord dh g2 d3 b3 e4)
   (mk-chord bar fs2 e3 a3 d4)  ;; Dadd9/F#
   ))

(define melody-b1
  (zip-notes
   (list
    q q q q
    q q q qr
    q q q q
    dq e q qr
    )
   (list
    a3 g4 fs4 d4
    a3 d4 fs4
    a3 g4 fs4 d4
    a4 fs4 d4 )))


(define melody-b2
  (zip-notes
   (list
    q q q q
    q q q qr
    q q q q
    dq e q qr
    )

   (list
    e4 a4 d5 a4
    d4 g4 fs4
    e4 a4 d5 a4
    fs5 e5 d5
    )))



;; (define section-b-rhy (repeat 2 w q dh))
(define section-b-rhy (repeat 4 (+ w q) dh))

(define section-b-v1-rhy (repeat 4 h hr))

(generate-midi
 'b
 (list

  (mk-track "Bassoon 1" (project-notes  melody-b1))
  (mk-track "Clarinet 1" (project-notes  (cons tacet4 melody-b2)))

  ;; (mk-track "Clarinet 1" (project-chords section-b-rhy chords-b fourth va8))
  ;; (mk-track "Clarinet 2" (project-chords section-b-rhy chords-b third ))
  ;; (mk-track "Bassoon 1" (project-chords section-b-rhy chords-b second ))
  (mk-track "Bassoon 2" (project-chords (repeat 4 w w) chords-b first ))

  ;; (mk-track "Horn 1" (project-chords section-b-v1-rhy chords-b fourth ))
  ;; (mk-track "Horn 2" (project-chords section-b-rhy chords-b third ))
  ;; (mk-track "Horn 3" (project-chords section-b-rhy chords-b second ))
  ;; (mk-track "Trombone 1" (project-chords (repeat 4 w w) chords-b first ))

  ;; (mk-track "Violins 1" (project-chords section-b-rhy chords-b fourth va8 ))
  ;; (mk-track "Violins 2" (project-chords section-b-rhy chords-b second va8 ))
  (mk-track "Viola" (project-chords section-b-rhy chords-b second ))
  (mk-track "Cello" (project-chords (repeat 4 w w) chords-b first ))

  ))

;; Section C
;; Chords - Dadd4, Gadd9

(define section-c-rhy
  (repeat 2 (list
             h h
             h h
             ;; q h er e
             )))

(define section-c-bass-rhy
  (repeat 2 (list q w dh)))

(define chords-a2
  (list
   (major dbar g3)
   (inv (major dbar d3))
   (minor dbar e3)        ;; dissonance c/b with melody -8vb; sounds OK

   ;; (mk-chord bar a3 a3 c4)
   ;; (mk-chord bar c4 c4 e4)
   (minor bar a3)
   (major bar c4)

   ))
(define inner-colour
  (list
   (cons dbar a4)
   (cons dbar g4)
   (cons bar b4)
   (cons bar e4)
   (cons bar a4)
   (cons bar e4)
   ))
(generate-midi
 'c1
 (list

  ;; Melody onto V1/V2 in octaves
  (mk-track "Violins 1" (project-notes melody-a va8))
  (mk-track "Violins 2" (project-notes melody-a ))

  ;; Bassoon is filling cello's A-place
  (mk-track "Bassoon 1" (project-chords (repeat 2 section-c-bass-rhy) chords-a2 first vb8))

  (mk-track "Trombone 1" (project-chords (repeat 2 section-c-rhy) chords-a2 first ))

  ;; Violas take over A-V2, Cello takes over A-Viola, DB plays A-Cello (vb8)

  (mk-track "Violas" (project-chords (repeat 2 section-c-rhy) chords-a2 second va8))
  (mk-track "Cello" (project-chords (repeat 2 section-c-rhy) chords-a2 third ))

  (mk-track "Horn 2" (project-notes inner-colour))
  (mk-track "Double Bass" (project-chords (repeat 2 section-c-bass-rhy) chords-a2 first vb8))

  ))


