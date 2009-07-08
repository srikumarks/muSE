
(module Num (complex complex? add mul neg)
  
  ; Forward declare the module-private Complex class
  (define Complex (object))
  
  (define (complex? c) (isa? Complex c))

  (define (complex r i)
    (new (list Complex) 'real r 'imag i))

  (define (add x y)
    (cond
     ((complex? x) (x 'add y))
     ((complex? y) (y 'add x))
     ((and (number? x) (number? y)) (+ x y))
     (T (raise 'NotNumbersEx x y))))
  
  (define (mul x y)
    (cond 
      ((complex? x) (x 'mul y))
      ((complex? y) (y 'mul x))
      ((and (number? x) (number? y)) (* x y))
      (T (raise 'NotNumbersEx x y))))

  (define (neg x)
    (cond
      ((number? x) (- x))
      ((complex? x) (x 'neg))
      (T (raise 'NotANumberEx x))))    
  
  ; Implementation of Complex class' methods.
  (put* Complex
	'add  (fn (self value)
		  (cons
		   ((number? value) (complex (+ self.real value) self.imag))
		   ((complex? value) (complex (+ self.real value.real) 
					      (+ self.image value.imag)))
		   (T (self 'add (raise 'NotANumberEx value)))))
        'mul (fn (self value)
		 (cond
		  ((number? value) (complex (* self.real value) (* self.imag value)))
		  ((complex? value) (let ((x1 self.real) 
					  (y1 self.imag) 
					  (x2 value.real) 
					  (y2 value.imag))
				      (complex (- (* x1 x2) (* y1 y2)) (+ (* x1 y2) (* x2 y2)))))
		  (T (self 'mul (raise 'NotANumberEx value)))
					; We allow the value to be replaced in case of an exception.
		  ))
	'neg (fn (self)
		 (complex (- self.real) (- self.imag)))

	'show (fn (self)
		  (print (format self.real "+" self.imag "i"))))
  
  )


(define x (Num.complex 2 3))
(define y (Num.complex 4.5 7.3))

(print "1. Module Num is defined with the following exports - ")
(print "   " Num)
(print "2. Symbols x and y are defined to some complex numbers.")
(print "3. Use (c 'show) to print out a complex number c.")

