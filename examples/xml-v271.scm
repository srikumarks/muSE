; xml-v271.scm
;
; In v271, a native function called read-xml was added to muSE
; to read an xml node from a port and return it as an equivalent
; list structure - which is the same as what is accepted by 
; write-xml.
;
; Having the xml data as an expr is ok, but it is not very useful 
; unless you have a few functions that help you extract information
; from the structure fairly easily. This file provides such a library
; of functions and macros that let you describe a set of nodes
; you might be interested in and then run the query on an xml tree
; to extract the set as a list of nodes.
;
; An xml node has the structure -
;    (tag ((attr1 . value1) (attr2 . value2) ...) ...body...)
; Wherever a "node" is specified in the functions below, it
; is referring to such a node.

; (tag= node)
; Evaluates to the tag name of the node.
(define tag=
  (fn (node)
      (case node
        ((tag attrs . body) tag)
        (_ ()))))

; (attr= node attr)
; Evaluates to the value of the "attr" attribute of the "node".
; "attr" should be a symbol.
(define attr=
  (fn (node a)
      (case node
        ((tag attrs . body) (rest (assoc attrs a)))
        (_ ()))))

; (body= node)
; Evaluates to the body content of the node. This includes all
; sub-nodes and text content.
(define body=
  (fn (node)
      (case node
        ((tag attrs . body) body)
        (_ _))))


;============================================================
; Combinator library
;
; The following are a set of predicates and combinators that 
; together help you specify a node query fairly easily,
; in the style of xpath.
;
; A predicate expression evaluates to a function of the
; form @code fn (node) -> list-of-nodes @endcode and can be
; thought of as a predicate. The predicate, when applied
; to a node, succeeds by evaluating to a non-empty list of 
; nodes that satisfy it and fails by evaluating to ().
; 
; Combinators are functions that take other predicates
; as parameters and evaluate to a new combined predicate.
;============================================================


; (:tag name)
; A macro that takes a tag name symbol and evaluates to a
; predicate that succeeds if the node's tag name is the given
; symbol. Usage - (:tag address), (:tag fullname) etc.
(define :tag
  (let ((tagsym (fn (sym)
                    (fn (node)
                        (case node
                          ((tag attrs . body) (if (= tag sym) (list node) ()))
                          (_ ()))))))
    (fn '(sym)
        (tagsym sym))))

; (:attr name expr)
; A macro that takes the symbolic name of an attribute
; and constructs a predicate that succeeds on a node if
; it has such an attribute and if the expr (using the
; name symbol directly) evalautes to non-nil.
; Usage - (:attr firstname (= firstname "Kumar"))
(define :attr
  (fn '($sym $expr)
      (let (($pred (eval (list fn: (list 'this $sym) $expr))))
        (fn (node)
            (case node
              ((tag attrs . body) (case (assoc attrs $sym)
                                    (($a . $v) (if ($pred node $v) (list node) ()))
                                    (_ ())))
              (_ ()))))))

; (:attrs (sym1 sym2...) expr)
; A variant of :attr where all the given attribute symbols must be present
; and the expr can mention any of them.
(define :attrs
  (fn '($syms $expr)
      (let (($pred (eval (list fn: (list 'this $syms) $expr))))
        (fn (node)
            (case node
              ((tag attrs . body) (try
                                   (if ($pred node (map (fn: (sym) 
                                                             (case (assoc attrs sym)
                                                               ((a . v) v)
                                                               (() (raise 'AttrAbsent))))
                                                        $syms))
                                       (list node)
                                       ())
                                   ()))
              (_ ()))))))
      
; (:select ..predicates..)
; A combinator that evalautes to a predicate that yields the
; result of applying each of the argument predicates in the
; given order, on the result of the preceding predicate. This is
; useful for progressive filtering, when you're interested in
; the results of the successive predicates and not whether the
; root node satisfies the predicates or not. For that, use :and.
(define :select
  (let ((select-helper (fn (self result preds)
                           (if result
                               (if preds 
                                   (self self (apply join (map (first preds) result)) (rest preds))
                                   result)
                               ()))))
    (fn preds
        (fn (node)
            (select-helper select-helper (list node) preds)))))

; (:and ...predicates..)
; Evaluates to a predicate which will succeed only if all the given
; predicates succeed.
(define :and
  (fn preds
      (fn (node)
          (if (andmap (fn: (p) (p node)) preds)
              (list node)
              ()))))

; (:any ..predicates..)
; Sort of union of the predicate results.
(define :any
  (fn preds
      (fn (node)
          (apply join (map (fn: (p) (p node)) preds)))))

; (:or ..predicates..)
; Evalutes to a predicate that will succeed on a node if any of
; the given predicates succeeds.
(define :or
  (fn preds
      (fn (node)
          (if (ormap (fn: (p) (p node)) preds)
              (list node)
              ()))))
  
; (:not pred)
; Evaluates to a predicate which succeeds if pred fails and
; fails if pred succeeds.
(define :not
  (fn (pred)
      (fn (node)
          (if (pred node)
              ()
              (list node)))))

; (:child pred)
; Evalutes to a predicate that will give you all children of the
; node that satisfy pred.
; Usage - (:child (:tag name))
(define :child
  (fn (pred)
      (fn (node)
          (case node
            ((tag attrs . body) (apply join (map pred body)))
            (_ ())))))

; (:path ..predicates..)
; Evaluates to a predicate that successively looks down the hierarchy
; of the given node. For example, (:path (:tag a) (:tag b)) will yield
; all tag bs that are chidlren of tag as.
(define :path
  (let ((path-helper (fn (self result preds)
                         (case preds
                           (() result)
                           ((here . deeper) (self self (apply join (map here result)) deeper))))))
    (fn (root . children)
        (fn (node)
            (path-helper path-helper (root node) (map :child children))))))

; (:descendant pred)
; Evaluates to a predicate that will succeed on any descendant of the
; given node that satisfies pred.
; Usage - (:descendant (:tag name)) will look for any tag with name "name"
; which is a child of some descendant of the root node. The root node itself
; considered as well.
(define :descendant
  (let ((desc (fn (self pred result next)
                  (if next
                      (self self pred (join result (apply join (map pred next))) (apply join (map body= next)))
                      result))))
  (fn (pred)
      (fn (node)
          (case node
            ((tag attrs . body) (desc desc pred (pred node) body))
            (_ ()))))))

; (read-xml-file filename)
; Handy function to open a file, read the first xml node in it
; and close the file.
(define read-xml-file
  (fn (file)
      (let ((f (open-file file 'for-reading))
	    (tl (read-xml f)))
	(close f)
	tl)))

(print "(XML library loaded)")                    