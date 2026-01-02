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
           [(and (= meta-type #x01))
            (define txt (bytes->string/utf-8 mdata))
            (push! (evt abs-tick 'text #f txt #f))]
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
          [(text) (format "text ~a" a)]
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

(define (text-events-from-tracks tracks)
  (define texts '())
  (for ([evts tracks] [ti (in-naturals 0)])
    (for ([e evts])
      (when (eq? (evt-kind e) 'text)
        (set! texts (cons (list ti (evt-tick e) (evt-a e)) texts)))))
  (sort texts < #:key second))

(define (track-info-from-tracks tracks keep-suffix?)
  (define names (make-hash))
  (define funcs (make-hash))
  (define (parse-name n)
    (define parts (string-split n ":" #:trim? #t))
    (define base (if (pair? parts) (car parts) n))
    (define suffix (if (> (length parts) 1) (string-downcase (last parts)) ""))
    (define func
      (cond
        [(member suffix '("melody" "engine" "support" "bass")) (string->symbol suffix)]
        [else #f]))
    (values (if keep-suffix? n base) func))
  (for ([evts tracks] [ti (in-naturals 0)])
    (for ([e evts])
      (when (eq? (evt-kind e) 'track-name)
        (define-values (dname func) (parse-name (evt-a e)))
        (hash-set! names ti dname)
        (when func (hash-set! funcs ti func)))))
  (values names funcs))

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

(define (parse-bar-range s)
  (define parts (string-split s ":" #:trim? #t))
  (cond
    [(= (length parts) 2)
     (define a (string->number (first parts)))
     (define b (string->number (second parts)))
     (and (number? a) (number? b) (list a b))]
    [else #f]))

(define (parse-track-list s)
  (define parts (string-split s "," #:trim? #t))
  (define (to-int v)
    (and v (inexact->exact (floor v))))
  (define (parse-token t)
    (define tok (string-trim t))
    (cond
      [(string-contains? tok "-")
       (define bits (string-split tok "-" #:trim? #t))
       (and (= (length bits) 2)
            (let* ([a (to-int (string->number (first bits)))]
                   [b (to-int (string->number (second bits)))])
              (and (number? a) (number? b)
                   (if (<= a b)
                       (range a (add1 b))
                       (range b (add1 a))))))]
      [else
       (define n (to-int (string->number tok)))
       (and (number? n) (list n))]))
  (define nums
    (apply append
           (for/list ([p parts])
             (define v (parse-token p))
             (if v v '()))))
  (and (pair? nums) (remove-duplicates nums)))

(define (window-from-bars bars max-tick bars-limit bar-range)
  (define total (length bars))
  (define (bar-start idx)
    (if (and (>= idx 0) (< idx total)) (list-ref bars idx) 0))
  (define (bar-end idx)
    (if (and (>= idx 0) (< idx total)) (list-ref bars idx) max-tick))
  (cond
    [(and bar-range (>= (first bar-range) 1) (>= (second bar-range) (first bar-range)))
     (define start-idx (sub1 (inexact->exact (floor (first bar-range)))))
     (define end-idx (inexact->exact (floor (second bar-range))))
     (values (bar-start start-idx) (bar-end end-idx))]
    [(and bars-limit (>= bars-limit 1))
     (define end-idx (inexact->exact (floor bars-limit)))
     (values (bar-start 0) (bar-end end-idx))]
    [else (values 0 max-tick)]))

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

(define (write-svg path notes division time-sigs unified? track-names track-functions use-function-colors? svg-width svg-bars svg-bar-range text-events overtone-count overtone-bloom?)
  (define note-h 16)
  (define pad-x 140)
  (define pad-y 20)
  (define track-gap 14)
  (define track-title-h 12)
  (define text-lane-h 14)
  (define brass-color "#c7c962")
  (define strings-color "#c99762")
  (define woodwind-color "#2b7a5a")
  (define melody-color "#d06b6b")
  (define engine-color "#5a8ad0")
  (define support-color "#6bb68a")
  (define bass-color "#6b5aa6")
  (define shade-step 0.04)
  (define shade-floor 0.8)
  (define shade-map (make-hash))
  (define order-map (make-hash))
  (define (svg-escape s)
    (define s1 (regexp-replace* #rx"&" s "&amp;"))
    (define s2 (regexp-replace* #rx"<" s1 "&lt;"))
    (regexp-replace* #rx">" s2 "&gt;"))

  (define (hex->rgb s)
    (list (string->number (substring s 1 3) 16)
          (string->number (substring s 3 5) 16)
          (string->number (substring s 5 7) 16)))

  (define (byte->hex n)
    (define s (number->string n 16))
    (if (< (string-length s) 2) (string-append "0" s) s))

  (define (rgb->hex r g b)
    (define r8 (max 0 (min 255 (inexact->exact (round r)))))
    (define g8 (max 0 (min 255 (inexact->exact (round g)))))
    (define b8 (max 0 (min 255 (inexact->exact (round b)))))
    (string-append "#" (byte->hex r8) (byte->hex g8) (byte->hex b8)))

  (define (shade-color hex factor)
    (define rgb (hex->rgb hex))
    (define r (* (first rgb) factor))
    (define g (* (second rgb) factor))
    (define b (* (third rgb) factor))
    (rgb->hex r g b))

  (define (track-group tid)
    (define name (string-downcase (hash-ref track-names tid "")))
    (define func (and use-function-colors? (hash-ref track-functions tid #f)))
    (cond
      [(eq? func 'melody) 'melody]
      [(eq? func 'engine) 'engine]
      [(eq? func 'support) 'support]
      [(eq? func 'bass) 'bass]
      [(or (string-contains? name "trombone")
           (string-contains? name "trumpet")
           (string-contains? name "horn")
           (string-contains? name "tuba"))
       'brass]
      [(or (string-contains? name "violin")
           (string-contains? name "viola")
           (string-contains? name "cello")
           (string-contains? name "bass"))
       'strings]
      [(or (string-contains? name "flute")
           (string-contains? name "oboe")
           (string-contains? name "clarinet")
           (string-contains? name "bassoon"))
       'woodwind]
      [else 'other]))

  (define (track-base-color tid)
    (case (track-group tid)
      [(melody) melody-color]
      [(engine) engine-color]
      [(support) support-color]
      [(bass) bass-color]
      [(brass) brass-color]
      [(strings) strings-color]
      [(woodwind) woodwind-color]
      [else (svg-color tid)]))

  (define (track-color tid)
    (define factor (hash-ref shade-map tid 1.0))
    (shade-color (track-base-color tid) factor))

  (define (lerp a b t)
    (+ a (* (- b a) t)))

  (define (heat-color t)
    (define t1 (max 0 (min 1 t)))
    (cond
      [(<= t1 0.33)
       (define tt (/ t1 0.33))
       (rgb->hex (lerp 0 0 tt) (lerp 120 200 tt) (lerp 255 120 tt))]
      [(<= t1 0.66)
       (define tt (/ (- t1 0.33) 0.33))
       (rgb->hex (lerp 0 255 tt) (lerp 200 210 tt) (lerp 120 0 tt))]
      [else
       (define tt (/ (- t1 0.66) 0.34))
       (rgb->hex (lerp 255 220 tt) (lerp 210 60 tt) (lerp 0 40 tt))]))

  (define pre-legend-ids
    (if unified? (sort (remove-duplicates (map note-track notes)) <) '()))
  (define max-tick (if (null? notes) 0 (apply max (map note-end notes))))
  (define bars (bar-boundaries max-tick division time-sigs))
  (define-values (window-start window-end)
    (window-from-bars bars max-tick svg-bars svg-bar-range))
  (define window-ticks (max 1 (- window-end window-start)))
  (define content-width
    (if (and svg-width (> svg-width (* 2 pad-x)))
        (- svg-width (* 2 pad-x))
        (inexact->exact (ceiling (* window-ticks 0.25)))))
  (define px-per-tick (/ content-width window-ticks))
  (define width (+ pad-x pad-x content-width))
  (define (x-of tick) (+ pad-x (inexact->exact (floor (* (- tick window-start) px-per-tick)))))
  (define (rect w) (inexact->exact (max 1 (floor w))))
  (define (note-in-window? n)
    (define ns (note-start n))
    (define ne (note-end n))
    (and (< ns window-end) (> ne window-start)))

  (define track-views '())
  (define height (+ pad-y pad-y))
  (define base-height 0)
  (define legend-h 0)
  (define legend-ids '())
  (cond
    [unified?
     (define wnotes (filter note-in-window? notes))
     (define use-notes (if (null? wnotes) notes wnotes))
     (define min-p (if (null? use-notes) 0 (apply min (map note-pitch use-notes))))
     (define max-p (if (null? use-notes) 0 (apply max (map note-pitch use-notes))))
     (define pitch-range (+ 1 (- max-p min-p)))
     (define lane-h (+ track-title-h text-lane-h (* pitch-range note-h)))
     (define y0 (+ pad-y track-title-h))
     (define text-y (+ pad-y track-title-h))
     (define notes-y (+ text-y text-lane-h))
     (set! track-views (list (list 'all notes min-p max-p notes-y lane-h text-y)))
     (set! height (+ pad-y lane-h))
      (set! legend-ids pre-legend-ids)
     (set! legend-h (if (null? legend-ids) 0 (+ 10 (* (length legend-ids) 12))))]
    [else
     (define by-track (make-hash))
     (for ([n notes])
       (hash-set! by-track (note-track n) (cons n (hash-ref by-track (note-track n) '()))))
     (define track-ids (sort (hash-keys by-track) <))
     (define y pad-y)
     (for ([ti track-ids])
       (define tnotes (reverse (hash-ref by-track ti)))
       (define wnotes (filter note-in-window? tnotes))
       (define use-notes (if (null? wnotes) tnotes wnotes))
       (define min-p (apply min (map note-pitch use-notes)))
       (define max-p (apply max (map note-pitch use-notes)))
       (define pitch-range (+ 1 (- max-p min-p)))
       (define lane-h (+ track-title-h text-lane-h (* pitch-range note-h)))
       (define text-y (+ y track-title-h))
       (define notes-y (+ text-y text-lane-h))
       (set! track-views (cons (list ti tnotes min-p max-p notes-y lane-h text-y) track-views))
       (set! y (+ y lane-h track-gap)))
     (set! track-views (reverse track-views))
     (set! height (if (null? track-views) (+ pad-y pad-y) (- y track-gap)))])
  (set! base-height height)
  (define track-order (if unified? legend-ids (map first track-views)))
  (define group-counts (make-hash))
  (for ([tid track-order] [i (in-naturals 0)])
    (define grp (track-group tid))
    (define gi (hash-ref group-counts grp 0))
    (hash-set! group-counts grp (add1 gi))
    (hash-set! shade-map tid (max shade-floor (- 1.0 (* shade-step gi))))
    (hash-set! order-map tid i))

  (define (bar-bands)
    (define bands '())
    (define bar-count (length bars))
    (for ([i (in-range bar-count)])
      (define start (list-ref bars i))
      (define end (if (< (add1 i) bar-count) (list-ref bars (add1 i)) max-tick))
      (when (and (odd? i) (< start window-end) (> end window-start))
        (define s (max start window-start))
        (define e (min end window-end))
        (define x (x-of s))
        (define w (rect (* (- e s) px-per-tick)))
        (set! bands
              (cons (format "<rect x='~a' y='~a' width='~a' height='~a' fill='#f0f0f0'/>"
                            x pad-y w (- height pad-y))
                    bands))))
    (string-join (reverse bands) "\n"))

  (define (bar-labels)
    (define labels '())
    (define bar-count (length bars))
    (for ([i (in-range bar-count)])
      (define start (list-ref bars i))
      (when (and (< start window-end) (>= start window-start))
        (define x (+ (x-of start) 2))
        (set! labels
              (cons (format "<text x='~a' y='~a' font-size='10' fill='#666'>~a</text>"
                            x (- pad-y 4) (add1 i))
                    labels))))
    (string-join (reverse labels) "\n"))

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
        (when (and (< t max-tick) (>= t window-start) (< t window-end))
          (define x (x-of t))
          (set! lines (cons (format "<line x1='~a' y1='~a' x2='~a' y2='~a' stroke='#ddd' stroke-width='1'/>"
                                    x pad-y x (- height pad-y))
                            lines)))))
    (string-join (reverse lines) "\n"))

  (define (rect-y max-p y0 pitch rect-h)
    (define row-y (+ y0 (* (- max-p pitch) note-h)))
    (+ row-y (inexact->exact (floor (/ (- note-h rect-h) 2)))))

  (define (rect-at x y w h color opacity title class-name)
    (if title
        (format "<rect class='~a' x='~a' y='~a' width='~a' height='~a' fill='~a' fill-opacity='~a' stroke='#222' stroke-width='1.0'><title>~a</title></rect>"
                class-name x y w h color opacity (svg-escape title))
        (format "<rect class='~a' x='~a' y='~a' width='~a' height='~a' fill='~a' fill-opacity='~a' stroke='#222' stroke-width='1.0'/>"
                class-name x y w h color opacity)))

  (define (note-rect n max-p y0 pitch opacity rect-h title class-name)
    (define ns (note-start n))
    (define ne (note-end n))
    (define s (max ns window-start))
    (define e (min ne window-end))
    (if (>= s e)
        ""
        (let* ([x (x-of s)]
               [w (rect (* (- e s) px-per-tick))]
               [y (rect-y max-p y0 pitch rect-h)])
          (rect-at x y w rect-h (track-color (note-track n)) opacity title class-name))))

  (define (overtone-pitches base)
    (define offsets '(12 19 24 28 31 34))
    (for/list ([o offsets])
      (+ base o)))

  (define (note-rects n max-p y0)
    (define base (note-pitch n))
    (define base-h (- note-h 1))
    (define tid (note-track n))
    (define tname (hash-ref track-names tid #f))
    (define label (if tname tname (format "Track ~a" tid)))
    (define class-name (format "note track-~a" tid))
    (define rects (list (note-rect n max-p y0 base 0.85 base-h label class-name)))
    (if (and (number? overtone-count) (>= overtone-count 1) (<= base 72))
        (append
         rects
         (for/list ([p (overtone-pitches base)]
                    [i (in-naturals 0)]
                    #:when (and (< i overtone-count) (<= p 77)))
           (define overtone-label (format "~a (overtone ~a)" label (add1 i)))
           (define overtone-h
             (max 1 (inexact->exact (floor (* base-h (- 0.6 (* 0.1 i)))))))
           (note-rect n max-p y0 p (- 0.4 (* 0.05 i)) overtone-h overtone-label class-name)))
        rects))

  (define (bloom-elements tnotes max-p y0)
    (if (and overtone-bloom? (number? overtone-count) (>= overtone-count 1))
        (let ([items '()])
          (for ([n tnotes])
            (define ns (note-start n))
            (define ne (note-end n))
            (when (and (< ns window-end) (> ne window-start))
              (define base (note-pitch n))
              (when (<= base 72)
                (for ([p (overtone-pitches base)]
                      [i (in-naturals 0)]
                      #:when (and (< i overtone-count) (<= p 77)))
                  (define intensity (exp (* -0.6 i)))
                  (define color (heat-color intensity))
                  (define opacity (* 0.35 intensity))
                  (define scale (+ 0.7 (* 0.6 intensity)))
                  (define rx (* (/ division 2.0) px-per-tick scale))
                  (define ry (* note-h 1.5 scale))
                  (define mid (/ (+ ns ne) 2.0))
                  (define midc (min (max mid window-start) window-end))
                  (define cx (x-of midc))
                  (define cy (+ y0 (* (- max-p p) note-h) (/ note-h 2.0)))
                  (set! items
                        (cons (format "<ellipse cx='~a' cy='~a' rx='~a' ry='~a' fill='~a' fill-opacity='~a' filter='url(#bloom-blur)'/>"
                                      cx cy rx ry color opacity)
                              items))))))
          (string-join (reverse items) "\n"))
        ""))

  (define (unified-rects tnotes max-p y0)
    (define descs '())
    (for ([n tnotes])
      (define ns (note-start n))
      (define ne (note-end n))
      (define s (max ns window-start))
      (define e (min ne window-end))
      (when (< s e)
        (define tid (note-track n))
        (define base (note-pitch n))
        (define base-h (- note-h 1))
        (define tname (hash-ref track-names tid #f))
        (define label (if tname tname (format "Track ~a" tid)))
        (define class-name (format "note track-~a" tid))
        (set! descs (cons (list tid base s e 0.85 base-h label class-name) descs))
        (when (and (number? overtone-count) (>= overtone-count 1) (<= base 72))
          (for ([p (overtone-pitches base)]
                [i (in-naturals 0)]
                #:when (and (< i overtone-count) (<= p 77)))
            (define overtone-label (format "~a (overtone ~a)" label (add1 i)))
            (define overtone-h
              (max 1 (inexact->exact (floor (* base-h (- 0.6 (* 0.1 i)))))))
            (set! descs (cons (list tid p s e (- 0.4 (* 0.05 i)) overtone-h overtone-label class-name) descs))))))
    (define groups (make-hash))
    (for ([d descs])
      (define key (list (second d) (third d) (fourth d)))
      (hash-set! groups key (cons d (hash-ref groups key '()))))
    (define out '())
    (for ([key (in-list (hash-keys groups))])
      (define group
        (sort (hash-ref groups key)
              <
              #:key (λ (d) (hash-ref order-map (first d) 0))))
      (define pitch (first key))
      (define s (second key))
      (define e (third key))
      (define count (length group))
      (define base-x (+ pad-x (* (- s window-start) (exact->inexact px-per-tick))))
      (define total-w (* (- e s) (exact->inexact px-per-tick)))
      (define row-y (+ y0 (* (- max-p pitch) note-h)))
      (for ([d group] [i (in-naturals 0)])
        (define tid (first d))
        (define opacity (list-ref d 4))
        (define rect-h (list-ref d 5))
        (define title (list-ref d 6))
        (define class-name (list-ref d 7))
        (define slice-h (/ (exact->inexact (max 1 rect-h)) (max 1 count)))
        (define base-y (+ row-y (inexact->exact (floor (/ (- note-h rect-h) 2)))))
        (define y (+ base-y (* i slice-h)))
        (define h (max 1 slice-h))
        (set! out (cons (rect-at base-x y (max 1 total-w) h (track-color tid) opacity title class-name) out))))
    (string-join (reverse out) "\n"))

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
      (set! labels (cons (format "<text x='~a' y='~a' font-size='12' fill='#555' text-anchor='end'>~a</text>"
                                 (- pad-x 6) y (note-name p))
                         labels)))
    (string-join (reverse labels) "\n"))

  (define (track-block tv)
    (define ti (list-ref tv 0))
    (define tnotes (list-ref tv 1))
    (define min-p (list-ref tv 2))
    (define max-p (list-ref tv 3))
    (define y0 (list-ref tv 4))
    (define text-y (list-ref tv 6))
    (define rects
      (if (and unified? (eq? ti 'all))
          (unified-rects tnotes max-p y0)
          (string-join
           (filter (λ (s) (not (string=? s "")))
                   (apply append (map (λ (n) (note-rects n max-p y0)) tnotes)))
           "\n")))
    (define bloom (bloom-elements tnotes max-p y0))
    (define rows (pitch-row-bands min-p max-p y0))
    (define labels (pitch-labels min-p max-p y0))
    (define tname (and (not (eq? ti 'all)) (hash-ref track-names ti #f)))
    (define title-text
      (cond
        [(eq? ti 'all) ""]
        [tname (format "Track ~a: ~a" ti tname)]
        [else (format "Track ~a" ti)]))
    (define title (format "<text x='~a' y='~a' font-size='10' font-weight='bold' fill='#333'>~a</text>"
                          pad-x (- y0 3) title-text))
    (if (string=? title-text "")
        (string-append rows "\n" bloom "\n" labels "\n" rects)
        (string-append title "\n" rows "\n" bloom "\n" labels "\n" rects)))

  (define (legend-block)
    (if (or (null? legend-ids) (not unified?))
        ""
        (let* ([legend-text-x 22]
               [legend-swatch-x 8]
               [legend-y (+ base-height 8)]
               [items
                (for/list ([tid legend-ids] [i (in-naturals 0)])
                  (define y (+ legend-y (* i 12)))
                  (define tname (hash-ref track-names tid #f))
                  (define label (if tname (format "Track ~a: ~a" tid tname) (format "Track ~a" tid)))
                  (define text-items
                    (for/list ([t text-events]
                               #:when (and (= (first t) tid)
                                           (>= (second t) window-start)
                                           (< (second t) window-end)))
                      (define x (x-of (second t)))
                      (define txt (third t))
                      (format "<text x='~a' y='~a' font-size='12' fill='~a'>~a</text>"
                              x (+ y 8) (track-color tid) txt)))
                  (string-append
                   (format "<g class='legend track-~a' data-track='~a'><rect x='~a' y='~a' width='8' height='8' fill='~a'/>\
<text x='~a' y='~a' font-size='12' fill='#333'>~a</text>"
                           tid tid legend-swatch-x y (track-color tid)
                           legend-text-x (+ y 8) label)
                   "\n"
                   (string-join text-items "\n")
                   "</g>"))])
          (string-join items "\n"))))

  (define (legend-style)
    (if (or (null? legend-ids) (not unified?))
        ""
        (string-append
         "#midi-roll .notes .note{opacity:1;}\n"
         "#midi-roll[data-active] .notes .note{opacity:0.4;}\n"
         (string-join
          (for/list ([tid legend-ids])
            (format "#midi-roll[data-active='~a'] .notes .track-~a{opacity:1;}" tid tid))
          "\n"))))

  (set! height (+ base-height legend-h))
  (define bloom-defs
    (if overtone-bloom?
        "<defs><filter id='bloom-blur'><feGaussianBlur stdDeviation='6'/></filter></defs>\n"
        ""))
  (define svg
    (string-append
     "<?xml version='1.0' encoding='UTF-8'?>\n"
     (format "<svg id='midi-roll' xmlns='http://www.w3.org/2000/svg' width='~a' height='~a' viewBox='0 0 ~a ~a'>\n"
             width height width height)
     (format "<rect x='0' y='0' width='~a' height='~a' fill='#fafafa'/>\n" width height)
     bloom-defs
     (format "<style>.legend{cursor:pointer;pointer-events:all;} ~a</style>\n" (legend-style))
     (bar-bands) "\n"
     (grid-lines) "\n"
     (bar-labels) "\n"
     (legend-block) "\n"
     "<g class='notes'>\n"
     (string-join (map track-block track-views) "\n")
     "\n</g>\n"
     "<script><![CDATA[\n"
     "const root=document.getElementById('midi-roll');\n"
     "const legends=root.querySelectorAll('.legend');\n"
     "for(const el of legends){\n"
     "  const tid=el.getAttribute('data-track');\n"
     "  el.addEventListener('mouseenter',()=>root.setAttribute('data-active',tid));\n"
     "  el.addEventListener('mouseleave',()=>root.removeAttribute('data-active'));\n"
     "}\n"
     "]]></script>\n"
     "\n</svg>\n"))
  (call-with-output-file path
    (λ (out) (display svg out))
    #:exists 'replace))

(define (usage)
  (displayln "Usage: racket midi_inspect.rkt <file.mid> [--notes] [--svg out.svg] [--svg-unified] [--svg-width N] [--svg-bars N] [--svg-bar-range A:B] [--svg-overtones N] [--svg-overtones-bloom] [--track-only LIST] [--track-except LIST] [--track-function] [--ascii] [--ascii-cols N]")
  (displayln "  --notes         Only print note-on/note-off events")
  (displayln "  --svg PATH      Write a piano-roll SVG to PATH")
  (displayln "  --svg-unified   Render a single piano roll for all tracks")
  (displayln "  --svg-width N   Target total SVG width in pixels (auto-scales time)")
  (displayln "  --svg-bars N    Render only the first N bars")
  (displayln "  --svg-bar-range A:B  Render bars A through B (1-based, inclusive)")
  (displayln "  --svg-overtones N Show N overtones (1-6, up to F5, if base <= C5)")
  (displayln "  --svg-overtones-bloom Render overtones as blurred heat-map bloom")
  (displayln "  --track-only LIST   Include only track numbers (e.g., 0,2,4 or 1-3)")
  (displayln "  --track-except LIST Exclude track numbers (e.g., 1,3 or 2-5)")
  (displayln "  --track-function    Enable function coloring and keep :suffix in names")
  (displayln "  --ascii         Print a piano-roll as ASCII")
  (displayln "  --ascii-cols N  Limit ASCII columns (auto-scales time)"))

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (define notes-only? (member "--notes" args))
  (define svg-idx (index-of args "--svg"))
  (define svg-path (and svg-idx (list-ref args (add1 svg-idx))))
  (define svg-unified? (member "--svg-unified" args))
  (define svg-overtones-idx (index-of args "--svg-overtones"))
  (define svg-overtones
    (let ([v (and svg-overtones-idx (string->number (list-ref args (add1 svg-overtones-idx))))])
      (and v (min 6 (max 1 v)))))
  (define svg-overtones-bloom? (member "--svg-overtones-bloom" args))
  (define svg-width-idx (index-of args "--svg-width"))
  (define svg-width
    (and svg-width-idx (string->number (list-ref args (add1 svg-width-idx)))))
  (define svg-bars-idx (index-of args "--svg-bars"))
  (define svg-bars
    (and svg-bars-idx (string->number (list-ref args (add1 svg-bars-idx)))))
  (define svg-bar-range-idx (index-of args "--svg-bar-range"))
  (define svg-bar-range
    (and svg-bar-range-idx (parse-bar-range (list-ref args (add1 svg-bar-range-idx)))))
  (define track-only-idx (index-of args "--track-only"))
  (define track-only
    (and track-only-idx (parse-track-list (list-ref args (add1 track-only-idx)))))
  (define track-except-idx (index-of args "--track-except"))
  (define track-except
    (and track-except-idx (parse-track-list (list-ref args (add1 track-except-idx)))))
  (define track-function? (member "--track-function" args))
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
  (define track-filter
    (cond
      [track-only (λ (i) (member i track-only))]
      [track-except (λ (i) (not (member i track-except)))]
      [else (λ (i) #t)]))
  (define filtered-tracks
    (for/list ([t tracks] [i (in-naturals 0)] #:when (track-filter i)) t))
  (displayln (format "Format ~a, Tracks ~a, Division ~a PPQ" fmt ntrks division))
  (for ([t filtered-tracks] [i (in-naturals 0)])
    (print-track i t notes-only?))
  (define notes (notes-from-tracks filtered-tracks))
  (define time-sigs (time-signatures-from-tracks filtered-tracks))
  (define-values (track-names track-functions)
    (track-info-from-tracks filtered-tracks track-function?))
  (define text-events (text-events-from-tracks filtered-tracks))
  (when svg-path
    (write-svg svg-path notes division time-sigs svg-unified? track-names track-functions track-function? svg-width svg-bars svg-bar-range text-events svg-overtones svg-overtones-bloom?)
    (displayln (format "Wrote ~a" svg-path)))
  (when ascii?
    (write-ascii notes division time-sigs ascii-cols)))
