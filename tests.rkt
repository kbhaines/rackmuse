#lang racket

(require rackunit "rackmuse.rkt")

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

(define (sel1 t d lst) (printf "~a:~a " (/ t q) d) (first lst))
(check-equal? (harmony->voice (list q qr q qr e e q q qr h h q) chord-seq sel1)
              '((960 . c) (-960 . 0) (960 . c) (-960 . 0) (480 . f) (480 . f) (960 . f) (960 . f2) (-960 . 0) (1920 . em) (1920 . em) (960 . em2)))

(check-equal? (harmony->voice (list h h) (list (cons q 'c4) (cons q 'c5) (cons q 'g4) (cons q 'g5)) #f)
              '((1920 . c4) (1920 . g4)))

(check-equal? (harmony->voice (list h h) (list (cons q 'c4) (cons q 'c5) (cons qr 0) (cons q 'g5)) #f)
              '((1920 . c4) (1920 . g4)))
