; Some examples of lazy evaluation.


; --------------------------------------------------
; Example 1: Infinite number sequence.
; Say you want to work with the sequence of numbers from
; 100 to you-don't-know-what in steps of 5.
; You can either do that by creating a wrapper
; function that abstracts the you-don't-know-what,
; and incur the massive memory load of creating a
; monster list only to use it one item at a time,
; .. or you can create it lazily like this -

(define (infinite-sequence n step)
  (lcons n (infinite-sequence (+ n step) step)))

; Now, evaluating -
;   (take 10 (infinite-sequence 1 2))
; will give you
;   (1 3 5 7 9 11 13 15 17 19)

; --------------------------------------------------
; Example 2: Repeating infinite list of items
; Say you have a list of items (1 2 3) and you
; want to derive an infinite list (1 2 3 1 2 3 ...)
; from it without modifying the original list and
; without creating a circular list. You can 
; do that using lazy evaluation like this -

(define (loop things iter)
  (if iter
    (lcons (first iter) (loop things (rest iter)))
    (loop things things)))

; Now evaluating -
;   (take 10 (loop '(1 2 3) ()))
; will give you
;   (1 2 3 1 2 3 1 2 3 1)


; --------------------------------------------------
; Example 2: Infinite random sequence of items
; Instead of choosing a looping pattern, you might
; want to pick at random from a given set of numbers
; and create a sequence out of it. You can create
; that lazily like this -

(define (random-sequence* thing-vec)
  (lcons (thing-vec (rand 0 (length thing-vec)))
         (random-sequence* thing-vec)))

(define (random-sequence thing-list)
  (random-sequence* (apply vector thing-list)))