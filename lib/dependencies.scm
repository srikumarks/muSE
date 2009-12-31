
; The 'dependencies' module is intended for use as a
; function to extract a list of references that a given
; object (typically a function) depends on.
;
; Call like -
;    (dependencies f)
; and get a list of objects. The entries of the returned list
; are either global symbols or lists containing a path to
; a definition via module names.
;
; For example -
;   (module M (x) (define x "earth"))
;   (define (greet planet) (fn (greeting) (format greeting " " planet "!")))
;   (dependencies (greet M.x))
;   > ((M x) format)
(module dependencies ()

  (define (scan-helper f h)
	(cond
	  ((not f) h)
	  ((cons? f) (scan-helper (rest f) (scan-helper (first f) h)))
	  ((lambda? f) (let ((s (symbol-whose-value-is f)))
					 (if s
					   (if (not (assoc h s))
						 (scan-helper (rest f) (cons (cons s T) h))
						 h)
					   (scan-helper (rest f) h))))
	  (T (let ((s (symbol-whose-value-is f)))
		   (if (and s (!= f s))
			 (cons (cons s T) h)
			 h)))))

  (define (main f)
	(map first (scan-helper f ())))

)

