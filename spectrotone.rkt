#lang racket

(provide spectrotone-color
         spectrotone-colors)

;; Named palette (hex) used by the spectrotone mappings.
(define spectrotone-colors
  (hash 'white  "#ffffff"
        'yellow "#fbc02d"
        'green  "#43a047"
        'blue   "#1e88e5"
        'orange "#f57c00"
        'red    "#d32f2f"
        'purple "#8e24aa"
        'brown  "#6d4c41"
        'gray   "#9e9e9e"
        'black  "#000000"))

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

(define (lerp a b t) (+ a (* (- b a) t)))

(define (mix c1 c2 t)
  (define rgb1 (hex->rgb c1))
  (define rgb2 (hex->rgb c2))
  (rgb->hex (lerp (first rgb1) (first rgb2) t)
            (lerp (second rgb1) (second rgb2) t)
            (lerp (third rgb1) (third rgb2) t)))

;; Each mapping: (instrument ranges)
;; ranges: list of (start end color1 color2) with inclusive MIDI note bounds.
(define spectrotone-maps
  (list
   (list "oboe"
         (list
          (list 58 67 'red 'orange)
          (list 68 79 'orange 'yellow)
          (list 80 91 'yellow 'yellow)))
   (list "violin"
         (list
          (list 55 62 'purple 'purple)
          (list 62 69 'blue 'blue)
          (list 69 76 'green 'green)
          (list 76 89 'yellow 'yellow)
          (list 89 100 'white 'white)))
   (list "viola"
         (list
          (list 48 55 'purple 'purple)
          (list 55 62 'blue 'blue)
          (list 62 69 'green 'green)
          (list 69 81 'yellow 'yellow)
          (list 81 93 'white 'white)))
   (list "cello"
         (list
          (list 36 43 'purple 'purple)
          (list 43 50 'blue 'blue)
          (list 50 57 'green 'green)
          (list 57 69 'yellow 'yellow)
          (list 70 81 'white 'white)))))

(define (spectrotone-color name pitch)
  (define lname (string-downcase name))
  (define (color-of sym) (hash-ref spectrotone-colors sym #f))
  (define (range-color r)
    (define lo (first r))
    (define hi (second r))
    (define c1 (color-of (third r)))
    (define c2 (color-of (fourth r)))
    (cond
      [(and (<= lo pitch) (<= pitch hi) c1 c2)
       (if (= lo hi)
           c1
           (mix c1 c2 (/ (- pitch lo) (max 1 (- hi lo)))))]
      [else #f]))
  (for/or ([m spectrotone-maps])
    (define inst (first m))
    (define ranges (second m))
    (and (string-contains? lname inst)
         (for/or ([r ranges]) (range-color r)))))
