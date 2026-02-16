#lang racket


(provide barchk)

(require
  (for-syntax syntax/parse)
  racket/syntax-srcloc)

(begin-for-syntax

  (define-syntax-class colon (pattern (~datum :)))
  (define-syntax-class ncolon (pattern (~not (~datum :))))

  (define-splicing-syntax-class pits
    (pattern (~seq sep:colon p:ncolon ... )
      #:attr sep-stx #'sep
      ;;#:attr ps (attribute p)
      ;;#:attr sum (for/sum ([t (in-list (attribute p))]) (duration->rat t))
      ))
  )
(define (chk l wherestx . args)
  (define sum (apply + (map abs args)))
  (unless (= l sum)
    (define where (srcloc->string (syntax-srcloc wherestx)))
    (error  'bars
            "~a: expected ~a but got ~a" where l sum

            ))
  args)

(define-syntax (barchk stx)
  (syntax-parse stx
    [(_ len:expr ps:pits ...)
     #`(append (chk len #'ps.sep-stx ps.p ...) ...)]
    ))


(define qbar 6/4)
(define qq 1/4)
(define qqt (* (/ qq 3) 2))

#;(/ q (* q 4))
#;(map (lambda(d)(* 4 d q))
       (barchk qbar
               : qq qq qqt qqt qqt qq qq
               : (* 4 qq))
       )
