#lang racket

(require
  "rackmuse.rkt"
  "tools.rkt"
  "mid.rkt")


(define bar (* 4 q ))
(define barr (- 0 bar))
(define dbar (* 2 bar))
(define dbarr (- 0 dbar))

;; (define s (/ e 2))

(define (dur d . args) (map (lambda(x)(cons d x)) args))

(define bpm 65)
(define key-sig '(-2 0) )
(define meter '(4 4))
(define filename-fmt "op46-~a.mid")
(define (generate-midi part-id tracks)
  (make-midi-track-file meter bpm key-sig (format filename-fmt part-id) tracks))

(define chords-c
  (list
   (mk-chord bar g2 bf2 d3)
   (mk-chord bar ef2 g2 bf2)
   (mk-chord bar f2 bf2 d3)
   (mk-chord bar f2 a2 c3)

   (mk-chord bar g2 bf2 d3)
   (mk-chord bar ef2 g2 bf2)

   ;; Bb F/c Gm/d Eb
   (mk-chord bar bf1 d2 f2)
   (mk-chord bar c2 f2 a2)
   (mk-chord bar d2 g2 bf2)
   (mk-chord bar ef2 g2 bf2)

   ;; Bb F/c Gm/d Eb
   (mk-chord bar bf1 d2 f2)
   (mk-chord bar c2 f2 a2)
   (mk-chord bar d2 g2 bf2)
   (mk-chord bar ef2 g2 bf2)

   (mk-chord bar f2 a2 c3)
   (mk-chord dbar bf2 d3 f3)
   ))

(define pulse-b-arp (arpeg '(2 2 0 1 0)))
(define pulse-b (repeat 2 (list e e q e e)))

(define rhythm-a2 (repeat 4 de sr))
(define rhythm-c2 (repeat 2 s s e sr s e))

(define/durpit melody-c1 4 4
  : q e er e e e e
  : q q q e er
  : q e er e e e e
  : h hr

  : q e er e e e e
  : h q q

  : q e dq q
  : h h
  : q e q e e e
  : h q qr

  : q e dq q
  : h h

  : q e q er e e
  : h h
  : h h
  : w
  ->
  : bf4 d5   ef5 d5 c5 bf4
  : g4 bf4 c5 d5
  : c5 f5   g5 f5 ef5 d5
  : c5

  : d5 f5   g5 f5 ef5 d5
  : bf4 d5 ef5

  : f5 a5 bf5 d5
  : c5 ef5
  : f5 a5 bf5 g5 f5 d5
  : ef5 f5

  : f5 a5 bf5 d5
  : c5 ef5

  : d5 f5 g5 f5 d5
  : ef5 f5
  : g5 a5
  : d5
  )



(define tacet4 (tacet bar 4))

(define e4-rhythm (repeat 2 s sr s s e e ))
(define section-c-rhy (append (repeat 15 rhythm-a2) (list dbar)))
(define section-c-blueprint
  (hash
   'a-mel:melody melody-c1
   'e1:engine (harmony->voice (append (repeat 5 rhythm-a2) (repeat 10 er q q q e)) chords-c third)
   'e2:engine (harmony->voice (repeat 15 rhythm-c2) chords-c second va8)
   'e3:engine (harmony->voice (repeat 15 (repeat 4 e er)) chords-c first va16)
   'e4:engine (harmony->voice (repeat 15 e4-rhythm) chords-c first va16)
   'b1:bass (harmony->voice section-c-rhy chords-c first)
   ))

(define section-c-orch
  (list
   (assign 'a-mel:melody "Trumpet 1" #f)
   (assign 'a-mel:melody "Horn 1" vb8)
   (assign 'a-mel:melody "Horn 2" vb8)
   (assign 'a-mel:melody "Trombone 1" vb8)

   (assign 'e3:engine "Trombone 2" vb8)

   (assign 'e4:engine "Violins 1" va8)
   (assign 'e2:engine "Violins 2" va8)
   (assign 'e3:engine "Violas" #f)
   ;; (assign 'a-mel:melody "Trombone 3" vb16)
   ;; (assign 'a-mel:melody "Horn 1" vb8)
   ;; (assign 'a-mel:melody "Horn 2" vb8)
   ;; (assign 'a-mel:melody "Horn 3" vb16)
   ;; (assign 'a-mel:melody "Horn 4" vb16)
   ;; (assign 'a+1:melody "Violins 1" #f)
   ;; (assign 'e2:engine "Violins 2" #f)
   ;; (assign 'c1:colour "Violas" #f)
   ;; (assign 'e1:engine "Trombone 2" #f)
   ;; (assign 'e1:engine "Trombone 3" #f)

   (assign 'b1:bass "Cellos" #f)
   (assign 'b1:bass "Double Bass" #f)
   ))

(generate-midi 'c (blueprint->midi section-c-blueprint))

(generate-midi 'c0 (blueprint->orchestrated-midi section-c-blueprint section-c-orch))


(define chord-a-arp (arpeg '(0 1 2 1 2 3 2 1)))

(define chords-a
  (list
   (mk-chord bar g2 bf2 d3 g3)
   (mk-chord bar g2 bf2 ef3 g3)
   (mk-chord bar f2 bf2 d3 f3)
   (mk-chord bar a2 c3 f3 a3)

   ;; (mk-chord bar g2 bf2 d3)
   ;; (mk-chord bar ef2 g2 bf2)
   ))

(define section-a-blueprint
  (hash
   'a-mel:melody melody-c1
   'e1:engine (harmony->voice (repeat 8 e e e e) chords-a chord-a-arp)
   ))
(generate-midi 'a (blueprint->midi section-a-blueprint))
