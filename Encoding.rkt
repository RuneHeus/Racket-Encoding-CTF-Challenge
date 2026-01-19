#lang racket

;;Performs a Caesar-style rotation on a single lowercase alphabetic character.
;;Example:
;;(rotate-char #\a 3) => #\d
;;(rotate-char #\z 1) => #\a
(define (rotate-char c n)
  (define a (char->integer #\a))
  (integer->char
   (+ a (modulo (+ (- (char->integer c) a) n) 26))))

;;Determines how much to rotate each character based on its position (index) in the string.
;;The cycle repeats every 5 characters
(define (shift-for-index i)
  (+ 3 (modulo i 5))) ; cycle: 3 4 5 6 7

;;Reorders a list of blocks (each block is a list of characters)
;;This is NOT a shuffle
;;It is deterministic and reversible.
;; Reordering rule:
;;   1. Take all EVEN-indexed blocks (0, 2, 4, ...)
;;   2. Then take all ODD-indexed blocks (1, 3, 5, ...)
;;   3. Concatenate them in that order
;;Example: blocks = (B0 B1 B2 B3) -> result = (B0 B2 B1 B3)
(define (reorder-blocks blocks)
  (append
   (for/list ([i (in-range (length blocks))] #:when (even? i))
     (list-ref blocks i))
   (for/list ([i (in-range (length blocks))] #:when (odd? i))
     (list-ref blocks i))))

(define (encoding str)
  ;;Creates a list of characters from given string
  ;;Example: "Hello World!" -> '(H e l l o W o r l d !) or (cons #\H (cons #\e (cons #\l (cons #\l (cons #\o (cons #\W (cons #\o (cons #\r (cons #\l (cons #\d (cons #\! '())))))))))))
  (define chars (string->list str))

  ;;This loop walks through the list of characters and applies a Caesar-style rotation to each one.
  (define rotated
    (for/list ([c chars] [i (in-naturals)])
      (rotate-char c (shift-for-index i))))

  ;; This uses a named `let` to implement a recursive loop that repeatedly takes the next 4 characters from the list.
  (define blocks
    (let loop ([lst rotated] [acc '()])
      (if (null? lst)
          (reverse acc)
          (let-values ([(block rest)
                        (split-at lst (min 4 (length lst)))])
            (loop rest (cons block acc))))))

  ;;Reorder the blocks and flatten them back into a single list
  (define reordered
    (apply append (reorder-blocks blocks)))
  
  ;;Convert the final list of characters back into a string
  (list->string reordered))

;;Deze moet nog weg, mag NIET in productie
(encoding "CTF{GoIng_TO_CaMp_Bl00d}")
