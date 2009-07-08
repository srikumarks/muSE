
; Since v238 on the trunk, a new primitive called "lcons"
; is available. Its behaviour is similar to "cons", except that
; it doesn't evaluate its arguments immediately. This gets us 
; the facility to implement infinite list based data structures.


; An infinite list of squares.
; You can do (take 10 (squares-from 1))
; to get the first 10 squares.
(define (squares-from N)
    (lcons (* N N) (squares-from (+ N 1))))
    
(print "The first 10 squares = " (take 10 (squares-from 1)))
    
; The infinite sequence of fibonacci numbers.
(define (fibs m n)
   (lcons n (fibs n (+ m n))))

(print "The first 10 fibonacci numbers = " (take 10 (fibs 0 1)))
        