#lang racket


(provide
 durchk
 pitchchk
 dur3/4 dur4/4 dur5/4 dur6/4 dur7/4
 dur3/8 dur4/8 dur5/8 dur6/8 dur7/8
 pitch3/4 pitch4/4 pitch5/4 pitch6/4 pitch7/4
 pitch3/8 pitch4/8 pitch5/8 pitch6/8 pitch7/8
 )

(require
  racket/syntax-srcloc
  "rackmuse.rkt"
  (for-syntax syntax/parse))


;; assumes 'q' is the basis of timing and that q = PPQ

(begin-for-syntax
  (define-syntax-class colon (pattern (~datum :)))
  (define-syntax-class ncolon (pattern (~not (~datum :))))
  (define-splicing-syntax-class pits
    (pattern (~seq sep:colon p:ncolon ... )
      #:attr sep-stx #'sep
      #:attr len (attribute p))))

(define (dur-chk num denom wherestx . args)
  (define sum (apply + (map abs args)))
  (define len (/ (* 4 PPQ num) denom))
  (define tsig (/ num denom))
  (unless (= len sum)
    (define where (srcloc->string (syntax-srcloc wherestx)))
    (define expected (/ num denom))
    (define got (/ sum PPQ 4))
    (error 'bars "~a: expected ~a but got ~a (need ~a)"
           where tsig got (- expected got)))
  args)

(define-syntax (durchk stx)
  (syntax-parse stx
    [(_ num:expr denom:expr ps:pits ...)
     #`(append (dur-chk num denom #'ps.sep-stx ps.p ...) ...)]
    ))

(define (slots-per-bar tsig durs)
  (for/fold
   ([acc '()][count 0][pos 0] #:result (reverse acc))
   ([d durs])
    (define barlen (* 4 PPQ tsig))
    (define pos! (+ pos (abs d)))
    (define count! (if (> d 0) (add1 count) count))
    (if (>= pos! barlen)
        (values (cons count! acc) 0 0)
        (values acc count! pos!))))

(define (pt-chk slots . args)
  (for/list ([s slots]
             [bp args]
             [i (in-naturals 1)])
    (define wherestx (car bp))
    (define pitches (cdr bp))
    (define sum (length pitches))
    (unless (= s sum)
      (define where (srcloc->string (syntax-srcloc wherestx)))
      (error 'bars "~a: expected ~a pitches, got ~a for bar ~a"
             where s sum i))
    pitches))

(define-syntax (pitchchk stx)
  (syntax-parse stx
    [(_ num:expr denom:expr durs:expr ps:pits ...)
     #`(flatten (pt-chk (slots-per-bar (/ num denom) durs) (list #'ps.sep-stx ps.p ...) ...))
     ]
    ))

(define-syntax-rule (dur3/4 args ...) (durchk 3 4 args ...))
(define-syntax-rule (dur4/4 args ...) (durchk 4 4 args ...))
(define-syntax-rule (dur5/4 args ...) (durchk 5 4 args ...))
(define-syntax-rule (dur6/4 args ...) (durchk 6 4 args ...))
(define-syntax-rule (dur7/4 args ...) (durchk 7 4 args ...))

(define-syntax-rule (dur3/8 args ...) (durchk 3 8 args ...))
(define-syntax-rule (dur4/8 args ...) (durchk 4 8 args ...))
(define-syntax-rule (dur5/8 args ...) (durchk 5 8 args ...))
(define-syntax-rule (dur6/8 args ...) (durchk 6 8 args ...))
(define-syntax-rule (dur7/8 args ...) (durchk 7 8 args ...))

(define-syntax-rule (pitch3/4 args ...) (pitchchk 3 4 args ...))
(define-syntax-rule (pitch4/4 args ...) (pitchchk 4 4 args ...))
(define-syntax-rule (pitch5/4 args ...) (pitchchk 5 4 args ...))
(define-syntax-rule (pitch6/4 args ...) (pitchchk 6 4 args ...))
(define-syntax-rule (pitch7/4 args ...) (pitchchk 7 4 args ...))

(define-syntax-rule (pitch3/8 args ...) (pitchchk 3 8 args ...))
(define-syntax-rule (pitch4/8 args ...) (pitchchk 4 8 args ...))
(define-syntax-rule (pitch5/8 args ...) (pitchchk 5 8 args ...))
(define-syntax-rule (pitch6/8 args ...) (pitchchk 6 8 args ...))
(define-syntax-rule (pitch7/8 args ...) (pitchchk 7 8 args ...))
