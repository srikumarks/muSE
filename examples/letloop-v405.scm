
(define letloop
  (let ((second (fn ((x1 x2 . etc)) x2)))
    (fn '(var arg-init-pairs . body)
      (list let ()
            (list local var)
            ; The following "forward declaration" is not needed from v405 onwards.
            ;(list define var (list fn '_))
            (cons define (cons (cons var (map first arg-init-pairs)) body))
            (cons var (map second arg-init-pairs))))))

(letloop f1 ((m 10))
         (print m)
         (if (= m 0)
             'done
             (f1 (- m 1))))

(letloop f1 ((m 10))
         (if (= m 0)
             'done
             (letloop f2 ((n 10))
                      (print m n)
                      (if (= n 0)
                          (f1 (- m 1))
                          (f2 (- n 1))))))

