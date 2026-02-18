#lang racket

(require
  rackunit
  "rackmuse.rkt"
  "tools.rkt")

(define chdur (list h h))
(check-equal? (durations->timeline (list qr e er q qr))
              '((0 . -960) (960 . 480) (1440 . -480) (1920 . 960) (2880 . -960)))
(check-equal? (durations->timeline chdur)
              '((0 . 1920) (1920 . 1920)))
(check-equal? (timeline-ref (durations->timeline (list qr e er q qr)) (+ 1440 480))
              '(1920 . 960))
(check-equal? (timeline-ref (durations->timeline chdur) 1920)
              '(1920 . 1920))
(check-equal? (timeline-index (durations->timeline chdur) (+ 3839 3840) #t)
              1)
(check-equal? (timeline-length (durations->timeline chdur))
              3840)

(define bar (* 4 q))
(define chord-seq
  (list
   (mk-chord bar 'c g3 b3 d4 a4)
   (mk-chord h 'f fs3 a3 d4 g4)
   (mk-chord h 'f2 fs3 a3 d4 a4)
   (mk-chord bar 'em e3 g3 b3 b4)
   (mk-chord bar 'em2 e3 g3 b3 e4)))

(define (sel1 t d lst) #;(printf "~a:~a " (/ t q) d) (first lst))

(check-equal? (harmony->voice (list q qr q qr e e q q qr h h q) chord-seq sel1)
              '((960 . c) (-960 . 0) (960 . c) (-960 . 0) (480 . f) (480 . f) (960 . f) (960 . f2) (-960 . 0) (1920 . em) (1920 . em) (960 . em2)))

(check-equal? (harmony->voice (list h h) (list (cons q 'c4) (cons q 'c5) (cons q 'g4) (cons q 'g5)) #f)
              '((1920 . c4) (1920 . g4)))

(check-equal?
 (harmony->voice (list h h) (list (cons q 'c4) (cons q 'c5) (cons qr 0) (cons q 'g5)) #f)
 '((1920 . c4) (1920 . 0)))

(check-not-exn (lambda()(durchk 3 8 : er er er : e e e : q er)))
(check-exn #rx"expected 1 but got 3/4" (lambda()(durchk 4 4 : q q qr q : q q q)))
(check-exn #rx"expected 5/4 but got 3/2 \\(need -1/4\\)" (lambda()(durchk 5 4 : q q q h : q qr qr h : hr hr hr)))

(check-equal?
 (pitchchk 4 4
           (durchk 4 4 : q q qr q : q hr q : e e er dq e e)
           : 1 2 1 : 4 5 : 8 9 10 11 12)
 '(1 2 1 4 5 8 9 10 11 12))

(check-equal?
 (pitchchk
  5 4
  (durchk 5 4 : q q q h : q qr qr h : hr hr qr : q hr hr)
  : 1 2 3 4 : 5 6 : : 7)
 '(1 2 3 4 5 6 7))

(check-exn #rx"expected 5 pitches, got 3 for bar 3"
           (lambda()(pitchchk 4 4 (list q q qr q  q hr q  e e er dq e e)
                              : 1 2 3 : 4 5 : 1 2 3 )))

(check-exn #rx"expected 5 pitches, got 6 for bar 3"
           (lambda()(pitchchk 4 4 (list q q qr q  q hr q  e e er dq e e)
                              : 1 2 3 : 4 5 : 1 2 3 4 5 6)))

(define/durpit c1 4 4
  : q q qr q
  : h h
  ->
  : 1 2 3
  : 4 5)

(check-equal? c1-durations '(960 960 -960 960 1920 1920))
(check-equal? c1-pitches '(1 2 3 4 5))
