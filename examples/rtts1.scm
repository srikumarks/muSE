(define *enable-rtts* T)

(define predicate-combinator
  (fn (mapper)
      (fn predicates
          (fn (x)
              (if (mapper (fn: (p) (p x)) predicates)
                  x
                  ())))))
      
(define ?or (predicate-combinator ormap))
(define ?and (predicate-combinator andmap))

(define ?not
  (fn (pred)
      (fn (x)
          (if (pred x) () x))))

(define ?list-of
  (fn (pred)
      (fn (x)
          (if x
              (if (if (cons? x) (andmap pred x) ())
                  x
                  ())
              T))))

(define number? (?or int? float?))

(define decltype
  (fn '(x type-spec)
      (if *enable-rtts*
          (eval (list 'fn x (list (eval type-spec) x)))
          x)))

