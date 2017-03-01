(defun getNamesIterator (arg)
  (cond
    ((NULL arg) nil)
    ((atom arg) arg)
    ((eq (car arg) '=) nil)
    (t (cons (car arg) (getNamesIterator (cdr arg))))
  )
)

(defun getNames (P)
  (let ((arg (cdar P)))
    (getNamesIterator arg)
  )
)

(defun getBodyIterator (arg)
  (cond
    ((NULL arg) nil)
    ((eq (car arg) '=) (cdr arg))
    (t  (getBodyIterator (cdr arg)))
  )
)
(defun getBody (P)
  (let ((arg (cdar P)))
    (car (getBodyIterator arg))
  )
)

(defun getValues (E)
 (cdr E)
)

(defun matchValueHelper (X N V)
  (cond
    ((NULL X) nil)
    ((NULL N) nil)
    ((eq X (car N)) t) ;If X is equal to the first element of N, return the corresponding V
    (t (matchValueHelper X (cdr N) (cdr V)))
  )
)

(defun locate (X N V)
  (cond
    ((eq X (car N)) (car V)) ;If X is equal to the first element of N, return the corresponding V
    (t (locate X (cdr N) (cdr V)))
  )

)
(defun matchValue (X N V)
  (let ((R (matchValueHelper X N V)))
    (if R
      (locate X N V)
       X
    )
  )
)

(defun buildExpr (B N V expr)
  ;Performs a search and replace of a body to make it evaluateable
  (cond
    ((NULL B) nil)
    ((ATOM (car B)) (append expr (list (matchValue (car B) N V)) (buildExpr (cdr B) N V nil)))
    ((NOT (ATOM (car B))) (append expr (list (buildExpr (car B) N V nil )) (buildExpr (cdr B) N V nil)))
    (t (buildExpr (cdr B) N V (append expr (list (matchValue (car B) N V)))))
    )
)

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
          ((eq f 'atom) (ATOM (car arg)))
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
          ((eq f fn) (flInterpHelper (buildExpr B N V nil) P B N Fn))
          ; Handle user defined functions
            ;something like ((lambda (vals) ((flInterpHelper (buildExpr B N vals nil) P))) findvalssomehow)
            ;
          (t  E)
          )
        )
      )
    )
  )

(defun fl-interp (E P)
  (cond
    ((atom E) E)
    (t
      (let ((N (getNames P)) (B (getBody P)) (fn (car E)))
        (flInterpHelper E P B N Fn)
      )
    )
  )
)
