#lang racket

;; Zet input om naar characters
(define (to-char x)
  (cond
    [(integer? x) (integer->char (+ 48 x))] ; 7 -> #\7
    [(symbol? x)  (string-ref (symbol->string x) 0)]
    [else x]))

;; Roteer een character via ASCII
(define (rotate-char ch n)
  (integer->char (+ (char->integer ch) n)))

;; Shift: 3 4 5 6 7
(define (shift-for-index i)
  (+ 3 (modulo i 5)))

;; Blokken herordenen: even eerst, dan oneven
(define (reorder-blocks blocks)
  (append
   (for/list ([i (in-range (length blocks))] #:when (even? i))
     (list-ref blocks i))
   (for/list ([i (in-range (length blocks))] #:when (odd? i))
     (list-ref blocks i))))

;; Split lijst in blokken van 4
(define (chunk4 lst)
  (if (null? lst)
      '()
      (cons (take lst (min 4 (length lst)))
            (chunk4 (drop lst (min 4 (length lst)))))))

;; Encoding
(define (encoding xs)
  (define chars (map to-char xs))

  (define rotated
    (for/list ([ch chars] [i (in-naturals)])
      (rotate-char ch (shift-for-index i))))

  (apply append (reorder-blocks (chunk4 rotated))))

(define (encoding->string xs)
  (list->string (encoding xs)))