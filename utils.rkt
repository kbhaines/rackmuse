#lang racket

(provide
 def/dur
 def/midi-gen
 )

(require
  "rackmuse.rkt"
  "mid.rkt"
  (for-syntax racket/syntax)
  )

(define-syntax (def/dur stx)
  (syntax-case stx ()
    [(_ id n d)
     (with-syntax
         ([dur (format-id #'id "~a" #'id)]
          [dur-rest (format-id #'id "~ar" #'id)])
       #'(begin
           (define dur (* n d))
           (define dur-rest (- 0 dur))))]
    ))

(define (def/midi-gen trackname bpm key-sig meter)

  (define (generate-midi part-id tracks)
    (define filename-fmt "~a-~a.mid")
    (make-midi-track-file meter bpm key-sig (format filename-fmt trackname part-id) tracks))

  generate-midi)
