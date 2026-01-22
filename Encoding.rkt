#lang racket

(define (to-char x)
  (cond
    [(integer? x) (integer->char (+ 48 x))]
    [(symbol? x)  (string-ref (symbol->string x) 0)]
    [else x]))

(define (rotate-char ch n)
  (integer->char (+ (char->integer ch) n)))

(define (shift-for-index i)
  (+ 3 (modulo i 5)))

(define (encoding xs)
  (define chars (map to-char xs))
  (for/list ([ch chars] [i (in-naturals)])
    (rotate-char ch (shift-for-index i))))

(define (encoding->string xs)
  (list->string (encoding xs)))

;; => Output = ":uk':h6{"
;; Can you reconstruct a valid input that produces this output?
;; The recovered input contains the secret you need.