#lang racket

(require
  "rackmuse.rkt"
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

(define chords-b
  (list
   (mk-chord bar g3 g3 bf3)
   (mk-chord bar f3 g3 c4)
   (mk-chord bar g3 bf3 d4)
   (mk-chord bar ef3 bf3 ef4)

   (mk-chord bar g3 bf3 d4)
   (mk-chord bar f3 c4 c4)
   (mk-chord bar g3 d4 d4)
   (mk-chord bar ef3 ef4 ef4)
   ))

(define pulse-b-arp (arpeg '(2 2 0 1 0)))
(define pulse-b (repeat 2 (list e e q e e)))

(define rhythm-a1 (repeat 2 h))
(define rhythm-a2 (repeat 4 de sr))
(define rhythm-b1 (repeat 4 sr s s s))
(define pitches-b1 (join (repeat 12 d4) (repeat 12 ef4) (repeat 12 f4) (repeat 12 g4)))
(define pitches-b2 (join  (repeat 12 g4) (repeat 12 a4) (repeat 12 bf4) (repeat 12 c5)))
;; (define pitches-b2 (join (repeat 8 f4) (repeat 8 g4) (repeat 8 a4) (repeat 8 bf4)))

(define melody-a1-pitches
  (list
   d5 ef5 f5 ef5
   c5 f5
   d5 ef5 g5 f5
   a5 bf5))

(define melody-a1-durs
  (list
   q q q er e
   q h qr
   q q q er e
   q h qr))

(define tacet4 (tacet bar 4))

(define section-c-blueprint
  (hash
   'a:melody (transpose vb8(cons tacet4 (zip-notes melody-a1-durs melody-a1-pitches)))
   'a+1:melody (cons tacet4 (zip-notes melody-a1-durs melody-a1-pitches))
   'sub-mel:support  (harmony->voice
                      (repeat 2 dh qr dh qr dh qr q h qr)
                      (zip-notes melody-a1-durs melody-a1-pitches)
                      #f vb8)
   'c1:colour (harmony->voice (repeat 8 rhythm-a2) chords-b second va8)
   'e1:engine (harmony->voice (repeat 8 rhythm-a2) chords-b third)
   'e2:engine (transpose va8 (zip-notes (repeat 8 rhythm-b1) (join pitches-b1 pitches-b2)))
   'b1:bass (harmony->voice (repeat 32 (list s sr s sr)) chords-b first)
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

(generate-midi 'bp-b (blueprint->midi section-c-blueprint))

(generate-midi 'b (blueprint->orchestrated-midi section-c-blueprint section-c-orch))



