#|Assignment 2 - fl-interp By Mackenzie Bligh
Overview:
  This code implements an interpreter for the fl language. First
  fl-interp is invoked, and causes flInterpHelper to be called.
  flInterpHelper evaluates primitive expressions, and calls buildExpr
  to build evaluateable fl expressions, which are then evaluated by
  flInterpHelper.
|#

#|getNamesIterator:
  Iterates through the list passed to it to find the names of variables
  . It builds a list of the values using cons, and returns it
|#
(defun getNamesIterator (arg)
  (cond
    ((NULL arg) nil)
    ((atom arg) arg)
    ((eq (car arg) '=) nil) ;If you've hit = stop searching
    ;Else keep searching for names
    (t (cons (car arg) (getNamesIterator (cdr arg))))
  )
)

#|getNames: Takes P as an argument
  gets the names of variables from a user defined function. Calls
  getNamesIterator to search through the expression and find the
  appropriate names. Returns a list of the names.
|#
(defun getNames (P)
  (let ((arg (cdar P))) ; Strip off function names
    (getNamesIterator arg) ; Search for names
  )
)

#|getBodyIterator: Takes P as an argument
  Iterates through the list passed to it to find the body of a user
  defined fl function It builds a list of the values using cons, and
  returns it.
|#
(defun getBodyIterator (arg)
  (cond
    ((NULL arg) nil)
    ; If you've hit = return the function body. Eg funct (x y) = (Do stuff)
    ; retunrs (Do stuff)
    ((eq (car arg) '=) (cdr arg))
    ; Else, keep searching
    (t  (getBodyIterator (cdr arg)))
  )
)

#|getBody: Takes P as an argument
  Gets the body of a user defined function. Calls
  getBodyIterator to search through the expression and find the
  appropriate names. Returns a function body of a user defined function.
  Will have it's variables exchanged for their values by buildExpr.
|#
(defun getBody (P)
  (let ((arg (cdar P))) ; Strip off useless information from parameters
    (car (getBodyIterator arg)) ; Search for function bodies
  )
)

#|getValues: Takes E as an argument
  Gets the values to be passed in to a user defined function.
  Returns a list of values.
|#
(defun getValues (E)
 (cdr E) ; Grab values from expression. Eg funct (x y) = (Do stuff)
         ; returns (x y)
)

#|matchValueHelper: Takes an atom, a name list and a values list as arguments
  Determines if there is a match for X in the name list (N).
|#
(defun matchValueHelper (X N V)
  (cond
    ((NULL X) nil)
    ((NULL N) nil)
    ((eq X (car N)) t) ; If X is equal to the first element of N, return the corresponding V
    (t (matchValueHelper X (cdr N) (cdr V))) ; Else, keep searching
  )
)
#|locate: Takes an atom, a name list, and a values list as arguments
  Locates the value of a variable X. It is only called if matchValueHelper finds a
  match. Returns a value as an atom
|#
(defun locate (X N V)
  (cond
    ((eq X (car N)) (car V)) ; If X is equal to the first element of N, return the corresponding V
    (t (locate X (cdr N) (cdr V))) ; Else, keep searching for a value of X 
  )

)
#|matchValue: Takes an atom, a name list, and a values list as arguments
  Determines if there is a match in the Name list for atom X. If there is a match
  it returns the value of the matching variable. If there is no match, it returns the
  atom X. It is called by buildExpr to perform subsitutions of variables for their values
  in the process of building an evaluateable fl expression.
|#
(defun matchValue (X N V)
  (let ((R (matchValueHelper X N V))) ; Determine if there is a matching variable
    (if R ; If there is a matching variable 
      (locate X N V) ; Return it's value
       X ; Else, return X
    )
  )
)

#|buildExpr: Takes a function body (B), name list (N), value list (V), and a list containing
  an executeable fl expresion.
  Builds an executeable fl expression by iterating through a function body, and subsituting
  variable names contained in N for their values contained in V. It calls matchValue to
  perform the subsitutions.
|#
(defun buildExpr (B N V expr)
  ;Performs a search and replace of a body to make it evaluateable
  (cond
    ((NULL B) nil) 
    ; If the first term in the body is an atom, start building an expression 
    ((ATOM (car B)) (append expr (list (matchValue (car B) N V)) (buildExpr (cdr B) N V nil)))
    ; Else if the first term in the body is a list, build an expression based on that 
    ; list and the rest of the body
    ((NOT (ATOM (car B))) (append expr (list (buildExpr (car B) N V nil )) (buildExpr (cdr B) N V nil)))
    ; Else build an expression 
    (t (buildExpr (cdr B) N V (append expr (list (matchValue (car B) N V)))))
    )
)

#|flInterpHelper: Takes an expression (E), parameters (P), function body (B), names list (N),
  and function name (Fn) as arguments.
  Evaluates primitives, and user defined functions, returns the result of evaluating the functions.
|#
(defun flInterpHelper (E P B N Fn)
  (cond
    ((atom E) E)
    (t
      (let ((f (car E))  (arg (cdr E)) (V (getValues E)))
        (cond
          ; handle built-in functions
          ((eq f 'if)
           (if (flInterpHelper (car arg) P B N Fn) (flInterpHelper (cadr arg) P B N Fn) (flInterpHelper (caddr arg) P B N Fn)))
          ((eq f 'null) (NULL (flInterpHelper (car arg) P B N Fn)))
          ((eq f 'atom) (ATOM (flInterpHelper (car arg) P B N Fn)))
          ((eq f 'eq) (eq (flInterpHelper (car arg) P B N Fn) (flInterpHelper (cadr arg) P B N Fn)))
          ((eq f 'first)  (car (flInterpHelper (car arg) P B N Fn)))
          ((eq f 'rest) (cdr (flInterpHelper (car arg) P B N Fn)))
          ((eq f 'cons) (cons (flInterpHelper (car arg) P B N Fn) (flInterpHelper (cadr arg) P B N Fn)))
          ((eq f 'equal) (equal (flInterpHelper (car arg) P B N Fn) (flInterpHelper (cadr arg) P B N Fn)))
          ((eq f 'isnumber) (numberp (flInterpHelper (car arg) P B N Fn)))
          ((eq f '+) (+ (flInterpHelper (car arg) P B N Fn) (flInterpHelper (cadr arg) P B N Fn)))
          ((eq f '-) (- (flInterpHelper (car arg) P B N Fn) (flInterpHelper (cadr arg) P B N Fn)))
          ((eq f '*) (* (flInterpHelper (car arg) P B N Fn) (flInterpHelper (cadr arg) P B N Fn)))
          ((eq f '>) (> (flInterpHelper (car arg) P B N Fn) (flInterpHelper (cadr arg) P B N Fn)))
          ((eq f '<) (< (flInterpHelper (car arg) P B N Fn) (flInterpHelper (cadr arg) P B N Fn)))
          ((eq f '=) (= (flInterpHelper (car arg) P B N Fn) (flInterpHelper (cadr arg) P B N Fn)))
          ((eq f 'and) (and (flInterpHelper (car arg) P B N Fn) (flInterpHelper (cadr arg) P B N Fn)))
          ((eq f 'or) (or (flInterpHelper (car arg) P B N Fn) (flInterpHelper (cadr arg) P B N Fn)))
          ((eq f 'not) (not (flInterpHelper (car arg) P B N Fn)))
          ; Handle user defined functions by building an expression
          ((eq f fn) (flInterpHelper (buildExpr B N V nil) P B N Fn)) 
          (t  E)
          )
        )
      )
    )
  )
#|fl-interp: takes an expression (E) and parameters (P) as arguments
  Calls flInterpHelper to evaluate the expression. It creates N, B, and fn
  to maintain the state of the names, function bodies, and function names across
  recursive calls of flInterpHelper
|#
(defun fl-interp (E P)
  (cond
    ((atom E) E)
    (t
      ;Define names, body and function name that should remain constant across calls
      (let ((N (getNames P)) (B (getBody P)) (fn (car E)))
        (flInterpHelper E P B N Fn)
      )
    )
  )
)
