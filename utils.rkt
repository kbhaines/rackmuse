#lang racket

(provide
 def/midi-gen
 )

(require
  "rackmuse.rkt"
  "mid.rkt"
  (for-syntax racket/syntax)
  )

(define (def/midi-gen trackname bpm key-sig meter)

  (define (generate-midi part-id tracks)
    (define filename-fmt "~a-~a.mid")
    (make-midi-track-file meter bpm key-sig (format filename-fmt trackname part-id) tracks))

  generate-midi)
