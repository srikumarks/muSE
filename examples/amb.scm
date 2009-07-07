; amb implementation taken from "Teach Yourself Scheme in fixnum days"
; by Dorai Sitaram 
; Sections 14.2 and 14.3

; (updated for muSE v372)

; muSE doesn't have backquote syntax, so here's a macro to help duplicate 
; the backquote facility that makes macro definitions simple to read.
(define LIT
  (fn '$xs
      (case $xs
        ((('UNLIT $y) . $ys) (list 'cons $y (apply LIT $ys)))
        ((('UNLIT@ $y) . $ys) (list 'join $y (apply LIT $ys)))
        (($y . $ys) (list 'cons (if (cons? $y) (apply LIT $y) (cons 'quote $y)) (apply LIT $ys)))
        ($y $y))))

; ---------------------------------------
; From section $14.2

(define amb-fail (box ()))
(define amb-fail-desc (box ()))

(define initialize-amb-fail
  (fn ()
      (amb-fail (fn +args 'AmbTreeExhausted))))

(initialize-amb-fail)

(define amb
  (fn '$alts...
      (LIT let ((+prev-amb-fail (amb-fail)))
           (call/cc (fn (+sk)
                        (UNLIT@ (map (fn (alt)
                                         (LIT amb-fail-desc 
                                              (call/cc (fn (+fk)
                                                          (amb-fail (fn +args 
                                                                        (amb-fail +prev-amb-fail)
                                                                        (+fk +args)))
                                                          (+sk (UNLIT alt))))))
                                     $alts...))
                        (apply +prev-amb-fail (amb-fail-desc)))))))

(define (fail . args) (amb-fail-desc args) (amb))

; ---------------------------------------
; Helpers 

(define assert
  (fn (pred)
      (if (not pred)
          (amb)
          ())))

; ---------------------------------------
; From section 14.3

(define bag-of
  (fn '($e)
      (LIT let ((+prev-amb-fail (amb-fail))
                (+results (box ())))
           (if (call/cc (fn (+k)
                            (amb-fail (fn +args (+k ())))
                            (let ((+v (UNLIT $e)))
                              (+results (cons +v (+results)))
                              (+k T))))
               ((amb-fail))
               ())
           (amb-fail +prev-amb-fail)
           (reverse! (+results)))))

