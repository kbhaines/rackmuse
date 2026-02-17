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
(define rhythm-c2 (repeat 4 s s e))

(define melody-c1-durs
  (dur4/4
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

   : q e q e e e
   : h h
   : h h
   : w
   ))

(define melody-c1-pitches
  (pitch4/4
   melody-c1-durs
   : bf4 d5   ef5 d5 c5 bf4
   : g4 bf4 c5 d5

   : c5 f5   g5 f5 ef5 d5
   : c5

   : d5 f5   g5 f5 ef5 d5
   : bf4 d5 ef5

   ;; m7
   : f5 a5 bf5 d5
   : c5 ef5

   ;; m9
   : f5 a5 bf5 g5 f5 d5
   : ef5 f5

   ;; m11
   : f5 a5 bf5 d5
   : c5 ef5

   ;; m13
   : f5 a5 bf5 g5 f5 d5
   : ef5 f5
   : g5 a5
   : d5
   ))



(define tacet4 (tacet bar 4))

(define e4-rhythm (repeat 2 s sr s s e e ))
(define section-c-rhy (append (repeat 15 rhythm-a2) (list dbar)))
(define section-c-blueprint
  (hash
   'a-mel:melody (zip-notes melody-c1-durs melody-c1-pitches)
   ;; 'a+1:melody (cons tacet4 (zip-notes melody-a1-durs melody-a1-pitches))
   ;; 'sub-mel:support  (harmony->voice
   ;;                    (repeat 2 dh qr dh qr dh qr q h qr)
   ;;                    (zip-notes melody-a1-durs melody-a1-pitches)
   ;;                    #f vb8)
   ;; 'c1:colour (harmony->voice (repeat 8 rhythm-a2) chords-c second va8)
   'e1:engine (harmony->voice (append (repeat 5 rhythm-a2) (repeat 10 er q q q e)) chords-c third)
   'e2:engine (harmony->voice (repeat 15 rhythm-c2) chords-c second va8)
   'e3:engine (harmony->voice (repeat 15 (repeat 4 e er)) chords-c first va16)
   'e4:engine (harmony->voice (repeat 15 e4-rhythm) chords-c first va16)
   ;; 'e2:engine (transpose va8 (zip-notes (repeat 8 rhythm-b1) (join pitches-b1 pitches-b2)))
   'b1:bass (harmony->voice section-c-rhy chords-c first)
   ))

(define section-c-orch
  (list
   (assign 'a:melody "Trombone 1" #f)
   (assign 'a:melody "Trombone 2" #f)
   (assign 'a:melody "Trombone 3" vb8)
   (assign 'a:melody "Horn 1" #f)
   (assign 'a:melody "Horn 2" #f)
   (assign 'a:melody "Horn 3" #f)
   (assign 'a:melody "Horn 4" #f)
   (assign 'a+1:melody "Violins 1" #f)
   ;; (assign 'e2:engine "Violins 2" #f)
   ;; (assign 'c1:colour "Violas" #f)
   ;; (assign 'e1:engine "Trombone 2" #f)
   ;; (assign 'e1:engine "Trombone 3" #f)

   (assign 'b1:bass "Cellos" #f)
   (assign 'sub-mel:support "Double Bass" #f)
   ))

(generate-midi 'c (blueprint->midi section-c-blueprint))

;; (generate-midi 'b (blueprint->orchestrated-midi section-c-blueprint section-c-orch))



