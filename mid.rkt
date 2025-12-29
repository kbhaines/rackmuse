#lang racket

;; PoC: Generate a multi-track Standard MIDI File (SMF) Format 1.
;; 4 bars, 4/4, 8th-note repetition of the chord tones:
;; C2 G2 E3 C4 (each on its own track/channel)
;;
;; Run: racket poc-midi.rkt
;; Output: poc.mid

(require racket/bytes)

(provide make-note-track2
         make-midi-track-file)

;; ---------- binary helpers ----------
(define (u16be n) (integer->integer-bytes n 2 #f #t))
(define (u32be n) (integer->integer-bytes n 4 #f #t))

;; MIDI variable-length quantity (VLQ) encoding for delta-times/lengths
(define (vlq n)
  (define (chunks n)
    (if (< n 128)
        (list n)
        (append (chunks (quotient n 128))
                (list (remainder n 128)))))
  (define cs (chunks n))
  (define lst (last cs))
  (define prefix (drop-right cs 1))
  (define bytes-list
    (append (map (λ (b) (bitwise-ior b #x80)) prefix)
            (list lst)))
  (apply bytes bytes-list))

(define (ascii-bytes s) (string->bytes/utf-8 s))

;; Build a MIDI track chunk: "MTrk" + length + data
(define (mtrk data)
  (bytes-append #"MTrk" (u32be (bytes-length data)) data))

;; Meta event: delta + 0xFF + type + len + data
(define (meta delta type data)
  (bytes-append (vlq delta) (bytes #xFF type) (vlq (bytes-length data)) data))

;; Channel voice events
(define (program-change delta chan program)
  (bytes-append (vlq delta) (bytes (+ #xC0 chan) program)))

(define (note-on delta chan note vel)
  (bytes-append (vlq delta) (bytes (+ #x90 chan) note vel)))

(define (note-off delta chan note vel)
  (bytes-append (vlq delta) (bytes (+ #x80 chan) note vel)))

;; Optional track name meta (nice for debugging)
(define (track-name delta s)
  (meta delta #x03 (ascii-bytes s)))

;; ---------- musical constants ----------
(define PPQ 480)              ; ticks per quarter note
(define q PPQ)
(define e (/ PPQ 2))          ; eighth note = 240 ticks
(define bars 4)
(define eighths (* bars 8))   ; 4 bars * 8 eighths per bar = 32

;; 120 bpm => 500000 microseconds per quarter note
(define tempo-us-per-q 500000)

;; ---------- track 0 (conductor/meta track) ----------
(define (make-meta-track nn dd [key-sig '(0 0)])
  (define sig (if (< (car key-sig) 0) (+ 256 (car key-sig)) (car key-sig)))
  (define data
    (bytes-append
     (track-name 0 "Conductor")
     ;; Tempo: FF 51 03 tt tt tt
     (meta 0 #x51 (bytes (bitwise-and (arithmetic-shift tempo-us-per-q -16) #xFF)
                         (bitwise-and (arithmetic-shift tempo-us-per-q -8)  #xFF)
                         (bitwise-and tempo-us-per-q #xFF)))
     ;; Time signature: FF 58 04 nn dd cc bb
     ;; nn=4, dd=2 (because 2^2=4), cc=24, bb=8
     ;;
     ;;4  nn  Numerator of the time signature (0-255)
     ;;5  dd  Denominator as a negative power of 2
     ;;6  cc  Number of MIDI clocks in a metronome click
     ;;7  bb  Number of notated 32nd notes in a MIDI quarter note
     ;;
     (meta 0 #x58 (bytes nn (inexact->exact (log dd 2)) 24 8))
     ;; Key signature: FF 59 02 sf mi  (C major => sf=0, mi=0)
     (meta 0 #x59 (bytes sig (cadr key-sig)))
     ;; End of track
     (meta 0 #x2F #"")))
  (mtrk data))

;; ---------- note tracks ----------
(define (make-note-track track-index chan note track-label)
  (define velocity 80)
  (define off-vel 64)
  (define data
    (let loop ([i 0]
               [acc (bytes-append (track-name 0 track-label)
                                  (program-change 0 chan 0))]) ; Acoustic Grand Piano
      (if (= i eighths)
          (bytes-append acc (meta 0 #x2F #"")) ; end of track
          (loop (add1 i)
                (bytes-append acc
                              (note-on 0 chan note velocity)
                              (note-off e chan note off-vel))))))
  (mtrk data))

(define (make-note-track2 chan note-data track-label)
  (define velocity 80)
  (define off-vel 64)
  (define hdr (bytes-append (track-name 0 track-label) (program-change 0 chan 0)))
  (define end (meta 0 #x2F #""))
  (define data
    (for/fold
     ([acc hdr]
      [lastp 0] #:result acc)
     ([n note-data])
      (define nd (caddr n))
      ;; (displayln (~a "nt:" nd ))
      (define note
        (cond
          [(integer? nd) nd]
          [else 127]))
      (define start (- (car n) lastp))
      (define len (cadr n))
      (define end (+ start len))
      ;; (displayln (~a "note:" n "start:" start ":" end " len:" len))
      (values
       (bytes-append acc (note-on start chan note velocity) (note-off len chan note off-vel))
       (+ (car n) len)
       )))
  (mtrk (bytes-append data end)))

(define (make-midi-track-file time-sig key-sig path tracks)
  (define ntrks (+ 1 (length tracks)))

  ;; MIDI header: "MThd" length=6 format=1 ntrks division
  (define header
    (bytes-append #"MThd"
                  (u32be 6)
                  (u16be 1)
                  (u16be ntrks)
                  (u16be PPQ)))

  (define trk0 (make-meta-track (car time-sig) (cadr time-sig) key-sig))

  ;; chord tones: C2=36, G2=43, E3=52, C4=60
  (define file-bytes
    (for/fold
     ([acc (bytes-append header trk0)]
      [chan 1] #:result acc)
     ([trkn tracks])
      (define trk-name (car trkn))
      (define trk (cdr trkn))
      (values
       (bytes-append acc (make-note-track2 chan trk trk-name))
       (add1 chan))))

  (displayln (bytes-length file-bytes))
  (call-with-output-file path
    (λ (out) (write-bytes file-bytes out))
    #:exists 'replace))

;; ---------- file assembly ----------
(define (make-midi-file path)
  (define ntrks 5) ; meta + 4 note tracks

  ;; MIDI header: "MThd" length=6 format=1 ntrks division
  (define header
    (bytes-append #"MThd"
                  (u32be 6)
                  (u16be 1)
                  (u16be ntrks)
                  (u16be PPQ)))

  (define trk0 (make-meta-track))

  ;; chord tones: C2=36, G2=43, E3=52, C4=60
  (define trk1 (make-note-track 1 0 36 "C2"))
  (define trk2 (make-note-track 2 1 43 "G2"))
  (define trk3 (make-note-track 3 2 52 "E3"))
  (define trk4 (make-note-track 4 3 60 "C4"))

  (define file-bytes (bytes-append header trk0 trk1))
  (call-with-output-file path
    (λ (out) (write-bytes file-bytes out))
    #:exists 'replace))

(module+ main
  (make-midi-file "poc.mid")
  (displayln "Wrote poc.mid"))
