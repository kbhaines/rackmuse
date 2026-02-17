#lang racket


(provide
 barchk
 align-chk
 bar3/4 bar4/4 bar5/4 bar6/4 bar7/4
 bar3/8 bar4/8 bar5/8 bar6/8 bar7/8
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

(define-syntax (barchk stx)
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

(define (expose slots . args)
  (for/list ([s slots]
             [bp args]
             [i (in-naturals 1)])
    (define wherestx (car bp))
    (define sum (length (cdr bp)))
    (unless (= s sum)
      (define where (srcloc->string (syntax-srcloc wherestx)))
      (error 'bars "~a: expected ~a pitches, got ~a for bar ~a"
             where s sum i))
    (cdr bp)))

(define-syntax (align-chk stx)
  (syntax-parse stx
    [(_ num:expr denom:expr durs:expr ps:pits ...)
     #`(flatten (expose (slots-per-bar (/ num denom) durs) (list #'ps.sep-stx ps.p ...) ...))
     ]
    ))




(define-syntax-rule (bar3/4 args ...) (barchk 3 4 args ...))
(define-syntax-rule (bar4/4 args ...) (barchk 4 4 args ...))
(define-syntax-rule (bar5/4 args ...) (barchk 5 4 args ...))
(define-syntax-rule (bar6/4 args ...) (barchk 6 4 args ...))
(define-syntax-rule (bar7/4 args ...) (barchk 7 4 args ...))

(define-syntax-rule (bar3/8 args ...) (barchk 3 8 args ...))
(define-syntax-rule (bar4/8 args ...) (barchk 4 8 args ...))
(define-syntax-rule (bar5/8 args ...) (barchk 5 8 args ...))
(define-syntax-rule (bar6/8 args ...) (barchk 6 8 args ...))
(define-syntax-rule (bar7/8 args ...) (barchk 7 8 args ...))
