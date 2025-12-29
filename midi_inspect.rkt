#lang racket

(require racket/bytes
         racket/list
         racket/string)

(struct evt (tick kind chan a b) #:transparent)
(struct note (track chan pitch start end) #:transparent)

(define (read-u16be b pos)
  (values (integer-bytes->integer (subbytes b pos (+ pos 2)) #f #t) (+ pos 2)))

(define (read-u32be b pos)
  (values (integer-bytes->integer (subbytes b pos (+ pos 4)) #f #t) (+ pos 4)))

(define (read-vlq b pos)
  (let loop ([p pos] [acc 0])
    (define v (bytes-ref b p))
    (define next (add1 p))
    (define acc* (+ (arithmetic-shift acc 7) (bitwise-and v #x7F)))
    (if (zero? (bitwise-and v #x80))
        (values acc* next)
        (loop next acc*))))

(define (note-name n)
  (define names '#("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))
  (define octave (- (quotient n 12) 1))
  (format "~a~a" (vector-ref names (modulo n 12)) octave))

(define (parse-track-data data)
  (define len (bytes-length data))
  (define pos 0)
  (define abs-tick 0)
  (define running #f)
  (define events '())
  (define (push! e) (set! events (cons e events)))
  (let loop ()
    (when (< pos len)
      (define-values (delta p1) (read-vlq data pos))
      (set! pos p1)
      (set! abs-tick (+ abs-tick delta))
      (define status (bytes-ref data pos))
      (define data1 #f)
      (cond
        [(< status #x80)
         (unless running
           (error 'parse-track-data "running status without prior status"))
         (set! data1 status)
         (set! status running)
         (set! pos (add1 pos))]
        [else
         (set! running status)
         (set! pos (add1 pos))])
      (cond
        [(= status #xFF)
         (define meta-type (bytes-ref data pos))
         (set! pos (add1 pos))
         (define-values (mlen p2) (read-vlq data pos))
         (set! pos p2)
         (define mdata (subbytes data pos (+ pos mlen)))
         (set! pos (+ pos mlen))
         (cond
           [(and (= meta-type #x03))
            (define name (bytes->string/utf-8 mdata))
            (push! (evt abs-tick 'track-name #f name #f))]
           [(and (= meta-type #x58) (>= (bytes-length mdata) 2))
            (define nn (bytes-ref mdata 0))
            (define dd (bytes-ref mdata 1))
            (define denom (expt 2 dd))
            (push! (evt abs-tick 'time-signature #f nn denom))]
           [(not (= meta-type #x2F))
            (push! (evt abs-tick 'meta #f meta-type (bytes-length mdata)))])]
        [(or (= status #xF0) (= status #xF7))
         (define-values (slen p2) (read-vlq data pos))
         (set! pos (+ p2 slen))
         (push! (evt abs-tick 'sysex #f slen #f))]
        [else
         (define kind (bitwise-and status #xF0))
         (define chan (bitwise-and status #x0F))
         (define (read-data1)
           (cond
             [data1 data1]
             [else (define v (bytes-ref data pos))
                   (set! pos (add1 pos))
                   v]))
         (define (read-data2)
           (define v (bytes-ref data pos))
           (set! pos (add1 pos))
           v)
         (define d1 (read-data1))
         (define d2 (if (or (= kind #xC0) (= kind #xD0))
                        #f
                        (read-data2)))
         (define ev-kind
           (case kind
             [(#x80) 'note-off]
             [(#x90) (if (and d2 (= d2 0)) 'note-off 'note-on)]
             [(#xA0) 'poly-pressure]
             [(#xB0) 'control-change]
             [(#xC0) 'program-change]
             [(#xD0) 'channel-pressure]
             [(#xE0) 'pitch-bend]
             [else 'unknown]))
         (push! (evt abs-tick ev-kind chan d1 d2))])
      (loop)))
  (reverse events))

(define (parse-midi path)
  (define b (call-with-input-file path port->bytes))
  (define pos 0)
  (define (read-chunk-id)
    (define id (bytes->string/utf-8 (subbytes b pos (+ pos 4))))
    (set! pos (+ pos 4))
    id)
  (define (read-chunk-data)
    (define-values (len p1) (read-u32be b pos))
    (set! pos p1)
    (define data (subbytes b pos (+ pos len)))
    (set! pos (+ pos len))
    data)

  (define hdr (read-chunk-id))
  (unless (string=? hdr "MThd")
    (error 'parse-midi "missing MThd header"))
  (define hdata (read-chunk-data))
  (define-values (fmt p1) (read-u16be hdata 0))
  (define-values (ntrks p2) (read-u16be hdata p1))
  (define-values (division _) (read-u16be hdata p2))

  (define tracks '())
  (for ([i (in-range ntrks)])
    (define tid (read-chunk-id))
    (unless (string=? tid "MTrk")
      (error 'parse-midi "missing MTrk at track ~a" i))
    (define tdata (read-chunk-data))
    (set! tracks (cons (parse-track-data tdata) tracks)))
  (values fmt ntrks division (reverse tracks)))

(define (print-track idx evts notes-only?)
  (define note-count 0)
  (define total (length evts))
  (for ([e evts])
    (when (memq (evt-kind e) '(note-on note-off))
      (set! note-count (add1 note-count))))
  (displayln (format "Track ~a: ~a events (~a note events)" idx total note-count))
  (for ([e evts])
    (define kind (evt-kind e))
    (unless (and notes-only?
                 (not (memq kind '(note-on note-off))))
      (define chan (evt-chan e))
      (define a (evt-a e))
      (define b (evt-b e))
      (define desc
        (case kind
          [(note-on note-off)
           (format "~a ch~a note ~a (~a) vel ~a"
                   kind chan a (note-name a) b)]
          [(program-change) (format "program-change ch~a program ~a" chan a)]
          [(control-change) (format "control-change ch~a cc ~a val ~a" chan a b)]
          [(meta) (format "meta type 0x~x len ~a" a b)]
          [(track-name) (format "track-name ~a" a)]
          [(time-signature) (format "time-signature ~a/~a" a b)]
          [(sysex) (format "sysex len ~a" a)]
          [else (format "~a ch~a a~a b~a" kind chan a b)]))
      (displayln (format "  t=~a  ~a" (evt-tick e) desc)))))

(define (notes-from-tracks tracks)
  (define open (make-hash)) ; key -> list of start ticks (stack)
  (define notes '())
  (define (key track chan pitch) (list track chan pitch))
  (for ([evts tracks] [ti (in-naturals 0)])
    (for ([e evts])
      (when (memq (evt-kind e) '(note-on note-off))
        (define k (key ti (evt-chan e) (evt-a e)))
        (if (eq? (evt-kind e) 'note-on)
            (hash-set! open k (cons (evt-tick e) (hash-ref open k '())))
            (let ([starts (hash-ref open k '())])
              (when (pair? starts)
                (define st (car starts))
                (hash-set! open k (cdr starts))
                (set! notes (cons (note ti (evt-chan e) (evt-a e) st (evt-tick e)) notes))))))))
  (reverse notes))

(define (time-signatures-from-tracks tracks)
  (define sigs '())
  (for ([evts tracks])
    (for ([e evts])
      (when (eq? (evt-kind e) 'time-signature)
        (set! sigs (cons (list (evt-tick e) (evt-a e) (evt-b e)) sigs)))))
  (sort sigs < #:key first))

(define (track-names-from-tracks tracks)
  (define names (make-hash))
  (for ([evts tracks] [ti (in-naturals 0)])
    (for ([e evts])
      (when (eq? (evt-kind e) 'track-name)
        (hash-set! names ti (evt-a e)))))
  names)

(define (bar-boundaries max-tick division time-sigs)
  (define sigs (if (null? time-sigs) (list (list 0 4 4)) time-sigs))
  (define bars '())
  (define sig-count (length sigs))
  (for ([i (in-range sig-count)])
    (define curr (list-ref sigs i))
    (define start (first curr))
    (define num (second curr))
    (define denom (third curr))
    (define bar-ticks (inexact->exact (/ (* division num 4) denom)))
    (define next-start
      (if (< i (sub1 sig-count)) (first (list-ref sigs (add1 i))) max-tick))
    (for ([t (in-range start (+ next-start bar-ticks) bar-ticks)])
      (when (< t max-tick)
        (set! bars (cons t bars)))))
  (sort bars <))

(define (write-ascii notes division time-sigs cols)
  (define max-tick (if (null? notes) 0 (apply max (map note-end notes))))
  (define ticks-per-col
    (cond
      [(and cols (> cols 0))
       (inexact->exact (max 1 (ceiling (/ max-tick cols))))]
      [else (max 1 (quotient division 4))]))
  (define total-cols (if (= max-tick 0) 0 (inexact->exact (ceiling (/ max-tick ticks-per-col)))))
  (define bars (bar-boundaries max-tick division time-sigs))
  (define bar-cols
    (for/list ([t bars] #:when (> t 0))
      (inexact->exact (quotient t ticks-per-col))))

  (define by-track (make-hash))
  (for ([n notes])
    (hash-set! by-track (note-track n) (cons n (hash-ref by-track (note-track n) '()))))
  (define track-ids (sort (hash-keys by-track) <))

  (for ([ti track-ids])
    (define tnotes (reverse (hash-ref by-track ti)))
    (define min-p (apply min (map note-pitch tnotes)))
    (define max-p (apply max (map note-pitch tnotes)))
    (displayln (format "Track ~a" ti))
    (define guide (make-string total-cols #\-))
    (for ([bc bar-cols] #:when (< bc total-cols))
      (string-set! guide bc #\|))
    (displayln (format "     ~a" guide))
    (for ([p (in-range max-p (sub1 min-p) -1)])
      (define row (make-string total-cols #\space))
      (for ([n tnotes] #:when (= (note-pitch n) p))
        (define start-col (inexact->exact (quotient (note-start n) ticks-per-col)))
        (define end-col (inexact->exact (ceiling (/ (note-end n) ticks-per-col))))
        (for ([i (in-range start-col (min end-col total-cols))])
          (string-set! row i #\#)))
      (for ([bc bar-cols] #:when (< bc total-cols))
        (when (char=? (string-ref row bc) #\space)
          (string-set! row bc #\|)))
      (displayln (format "~a ~a" (~a (note-name p) #:width 4) row)))
    (newline)))

(define (svg-color idx)
  (define colors
    '#("#1b9e77" "#d95f02" "#7570b3" "#e7298a" "#66a61e" "#e6ab02"))
  (vector-ref colors (modulo idx (vector-length colors))))

(define (write-svg path notes division time-sigs unified? track-names)
  (define px-per-tick 0.25)
  (define note-h 16)
  (define pad-x 60)
  (define pad-y 20)
  (define track-gap 14)
  (define track-title-h 12)
  (define max-tick (if (null? notes) 0 (apply max (map note-end notes))))
  (define width (+ pad-x pad-x (inexact->exact (ceiling (* max-tick px-per-tick)))))
  (define (x-of tick) (+ pad-x (inexact->exact (floor (* tick px-per-tick)))))
  (define (rect w) (inexact->exact (max 1 (floor w))))

  (define track-views '())
  (define height (+ pad-y pad-y))
  (define base-height 0)
  (define legend-h 0)
  (define legend-ids '())
  (cond
    [unified?
     (define min-p (if (null? notes) 0 (apply min (map note-pitch notes))))
     (define max-p (if (null? notes) 0 (apply max (map note-pitch notes))))
     (define pitch-range (+ 1 (- max-p min-p)))
     (define lane-h (+ track-title-h (* pitch-range note-h)))
     (define y0 (+ pad-y track-title-h))
     (set! track-views (list (list 'all notes min-p max-p y0 lane-h)))
     (set! height (+ pad-y lane-h))
     (set! legend-ids (sort (remove-duplicates (map note-track notes)) <))
     (set! legend-h (if (null? legend-ids) 0 (+ 10 (* (length legend-ids) 12))))]
    [else
     (define by-track (make-hash))
     (for ([n notes])
       (hash-set! by-track (note-track n) (cons n (hash-ref by-track (note-track n) '()))))
     (define track-ids (sort (hash-keys by-track) <))
     (define y pad-y)
     (for ([ti track-ids])
       (define tnotes (reverse (hash-ref by-track ti)))
       (define min-p (apply min (map note-pitch tnotes)))
       (define max-p (apply max (map note-pitch tnotes)))
       (define pitch-range (+ 1 (- max-p min-p)))
       (define lane-h (+ track-title-h (* pitch-range note-h)))
       (define y0 (+ y track-title-h))
       (set! track-views (cons (list ti tnotes min-p max-p y0 lane-h) track-views))
       (set! y (+ y lane-h track-gap)))
     (set! track-views (reverse track-views))
     (set! height (if (null? track-views) (+ pad-y pad-y) (- y track-gap)))])
  (set! base-height height)

  (define (bar-bands)
    (define sigs (if (null? time-sigs) (list (list 0 4 4)) time-sigs))
    (define bands '())
    (define sig-count (length sigs))
    (for ([i (in-range sig-count)])
      (define curr (list-ref sigs i))
      (define start (first curr))
      (define num (second curr))
      (define denom (third curr))
      (define bar-ticks (inexact->exact (/ (* division num 4) denom)))
      (define next-start
        (if (< i (sub1 sig-count)) (first (list-ref sigs (add1 i))) max-tick))
      (for ([t (in-range start (+ next-start bar-ticks) bar-ticks)]
            [bi (in-naturals 0)])
        (when (and (odd? bi) (< t max-tick))
          (define x (x-of t))
          (define w (rect (* bar-ticks px-per-tick)))
          (set! bands
                (cons (format "<rect x='~a' y='~a' width='~a' height='~a' fill='#f0f0f0'/>"
                              x pad-y w (- height pad-y))
                      bands)))))
    (string-join (reverse bands) "\n"))

  (define (grid-lines)
    (define sigs (if (null? time-sigs) (list (list 0 4 4)) time-sigs))
    (define lines '())
    (define sig-count (length sigs))
    (for ([i (in-range sig-count)])
      (define curr (list-ref sigs i))
      (define start (first curr))
      (define denom (third curr))
      (define beat-ticks (inexact->exact (/ (* division 4) denom)))
      (define next-start
        (if (< i (sub1 sig-count)) (first (list-ref sigs (add1 i))) max-tick))
      (for ([t (in-range start (+ next-start beat-ticks) beat-ticks)])
        (when (< t max-tick)
          (define x (x-of t))
          (set! lines (cons (format "<line x1='~a' y1='~a' x2='~a' y2='~a' stroke='#ddd' stroke-width='1'/>"
                                    x pad-y x (- height pad-y))
                            lines)))))
    (string-join (reverse lines) "\n"))

  (define (note-rect n max-p y0)
    (define x (x-of (note-start n)))
    (define w (rect (* (- (note-end n) (note-start n)) px-per-tick)))
    (define y (+ y0 (* (- max-p (note-pitch n)) note-h)))
    (format "<rect x='~a' y='~a' width='~a' height='~a' fill='~a' fill-opacity='0.85' stroke='#222' stroke-width='0.5'/>"
            x y w (- note-h 1) (svg-color (note-track n))))

  (define (black-key? pitch)
    (member (modulo pitch 12) '(1 3 6 8 10)))

  (define (pitch-row-bands min-p max-p y0)
    (define bands '())
    (define lines '())
    (for ([p (in-range max-p (sub1 min-p) -1)]
          [i (in-naturals 0)])
      (define y (+ y0 (* i note-h)))
      (when (black-key? p)
        (set! bands
              (cons (format "<rect x='~a' y='~a' width='~a' height='~a' fill='#e6e6e6'/>"
                            pad-x y (- width pad-x) note-h)
                    bands)))
      (set! lines
            (cons (format "<line x1='~a' y1='~a' x2='~a' y2='~a' stroke='#ddd' stroke-width='1'/>"
                          pad-x y (- width pad-x) y)
                  lines)))
    (string-append (string-join (reverse bands) "\n")
                   (if (null? bands) "" "\n")
                   (string-join (reverse lines) "\n")))

  (define (pitch-labels min-p max-p y0)
    (define labels '())
    (for ([p (in-range max-p (sub1 min-p) -1)])
      (define y (+ y0 (* (- max-p p) note-h) (- note-h 2)))
      (set! labels (cons (format "<text x='6' y='~a' font-size='12' fill='#555'>~a</text>"
                                 y (note-name p))
                         labels)))
    (string-join (reverse labels) "\n"))

  (define (track-block tv)
    (define ti (list-ref tv 0))
    (define tnotes (list-ref tv 1))
    (define min-p (list-ref tv 2))
    (define max-p (list-ref tv 3))
    (define y0 (list-ref tv 4))
    (define rects (string-join (map (λ (n) (note-rect n max-p y0)) tnotes) "\n"))
    (define rows (pitch-row-bands min-p max-p y0))
    (define labels (pitch-labels min-p max-p y0))
    (define tname (and (not (eq? ti 'all)) (hash-ref track-names ti #f)))
    (define title-text
      (cond
        [(eq? ti 'all) "All tracks"]
        [tname (format "Track ~a: ~a" ti tname)]
        [else (format "Track ~a" ti)]))
    (define title (format "<text x='~a' y='~a' font-size='10' font-weight='bold' fill='#333'>~a</text>"
                          pad-x (- y0 3) title-text))
    (string-append title "\n" rows "\n" labels "\n" rects))

  (define (legend-block)
    (if (or (null? legend-ids) (not unified?))
        ""
        (let* ([legend-x pad-x]
               [legend-y (+ base-height 8)]
               [items
                (for/list ([tid legend-ids] [i (in-naturals 0)])
                  (define y (+ legend-y (* i 12)))
                  (define tname (hash-ref track-names tid #f))
                  (define label (if tname (format "Track ~a: ~a" tid tname) (format "Track ~a" tid)))
                  (format "<rect x='~a' y='~a' width='8' height='8' fill='~a'/>\
<text x='~a' y='~a' font-size='9' fill='#333'>~a</text>"
                          legend-x y (svg-color tid)
                          (+ legend-x 12) (+ y 8) label))])
          (string-join items "\n"))))

  (set! height (+ base-height legend-h))
  (define svg
    (string-append
     "<?xml version='1.0' encoding='UTF-8'?>\n"
     (format "<svg xmlns='http://www.w3.org/2000/svg' width='~a' height='~a' viewBox='0 0 ~a ~a'>\n"
             width height width height)
     (format "<rect x='0' y='0' width='~a' height='~a' fill='#fafafa'/>\n" width height)
     (bar-bands) "\n"
     (grid-lines) "\n"
     (legend-block) "\n"
     (string-join (map track-block track-views) "\n")
     "\n</svg>\n"))
  (call-with-output-file path
    (λ (out) (display svg out))
    #:exists 'replace))

(define (usage)
  (displayln "Usage: racket midi_inspect.rkt <file.mid> [--notes] [--svg out.svg] [--svg-unified] [--ascii] [--ascii-cols N]")
  (displayln "  --notes         Only print note-on/note-off events")
  (displayln "  --svg PATH      Write a piano-roll SVG to PATH")
  (displayln "  --svg-unified   Render a single piano roll for all tracks")
  (displayln "  --ascii         Print a piano-roll as ASCII")
  (displayln "  --ascii-cols N  Limit ASCII columns (auto-scales time)"))

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (define notes-only? (member "--notes" args))
  (define svg-idx (index-of args "--svg"))
  (define svg-path (and svg-idx (list-ref args (add1 svg-idx))))
  (define svg-unified? (member "--svg-unified" args))
  (define ascii? (member "--ascii" args))
  (define ascii-cols-idx (index-of args "--ascii-cols"))
  (define ascii-cols
    (and ascii-cols-idx
         (string->number (list-ref args (add1 ascii-cols-idx)))))
  (define path (for/first ([a (in-list args)] #:unless (string-prefix? a "--")) a))
  (unless path
    (usage)
    (exit 1))

  (define-values (fmt ntrks division tracks) (parse-midi path))
  (displayln (format "Format ~a, Tracks ~a, Division ~a PPQ" fmt ntrks division))
  (for ([t tracks] [i (in-naturals 0)])
    (print-track i t notes-only?))
  (define notes (notes-from-tracks tracks))
  (define time-sigs (time-signatures-from-tracks tracks))
  (define track-names (track-names-from-tracks tracks))
  (when svg-path
    (write-svg svg-path notes division time-sigs svg-unified? track-names)
    (displayln (format "Wrote ~a" svg-path)))
  (when ascii?
    (write-ascii notes division time-sigs ascii-cols)))
