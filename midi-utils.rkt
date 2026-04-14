#lang racket

(provide
 def/midi-gen
 render-svg
 render-svg-overtones-bloom
 render-svg-spectrotone-overtones-bloom
 )

(require
  "rackmuse.rkt"
  "mid.rkt"
  "midi-inspect.rkt"
  (for-syntax racket/syntax)
  )

(define (def/midi-gen trackname bpm key-sig meter)

  (define (generate-midi part-id tracks [render? #f])
    (define filename (format "~a-~a.mid" trackname part-id))
    (make-midi-track-file meter bpm key-sig filename tracks)
    (when render? (render? filename (format "~a-~a.svg" trackname part-id)))
    filename)
  generate-midi)

(define (render-svg/config midi-file output-file #:overtones? [overtones? 0] #:spectrotone? [spectrotone? #f])
  (define-values (_fmt _ntrks division tracks) (parse-midi midi-file))
  (define notes (notes-from-tracks tracks))
  (define time-sigs (time-signatures-from-tracks tracks))
  (define-values (track-names track-functions)
    (track-info-from-tracks tracks #f))
  (define text-events (text-events-from-tracks tracks))
  (define svg
    (generate-svg notes division time-sigs #t track-names track-functions #f
                  #:text-events text-events
                  #:overtone-count overtones?
                  #:overtone-bloom? #t
                  #:spectrotone? spectrotone?))
  (call-with-output-file output-file
    (lambda (out) (display svg out))
    #:exists 'replace))

(define (render-svg-overtones-bloom midi-file output-file)
  (render-svg/config midi-file output-file #:overtones? 1))

(define (render-svg-spectrotone-overtones-bloom midi-file output-file)
  (render-svg/config midi-file output-file #:overtones? 1 #:spectrotone? #t))

(define (render-svg midi-file output-file)
  (render-svg/config midi-file output-file ))
