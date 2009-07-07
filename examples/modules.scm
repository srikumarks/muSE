;
; Usage: (module (sym1 val1) (sym2 val2) ...)
; Evaluates to a module object, so you can do
;   (define MyModule (module (sym1 val1) (sym2 val2) ...))
; and thereafter use MyModule like
; {MyModule ...expressions...}
;
(define module
  (fn 'bindings
    (let ((evaluated-bindings 
            (eval (list let bindings 
                    (list map 
                          (fn: ((s . _)) 
                            (list s (cons quote (eval s)))) 
                          (cons quote bindings))))))
      (fn 'args
        (cons let (cons evaluated-bindings args))))))

;
; Scoping operator
; Usage: {:: Module symbol}
; Resolves the value of the symbol in the context of the given
; module.
;
(define :: (fn 'args (cons quote (eval (eval args)))))

