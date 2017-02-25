#|
(defun locate (X L M)

)

(defun assoc (X N V)

)

(defun eval

)

|#
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
    ((eq X (car N)) (car V)) ;If X is equal to the first element of N, return the corresponding V
    (t (matchValueHelper X (cdr N) (cdr V)))
  )
)

(defun matchValue (X N V)
  (let ((R (matchValueHelper X N V)))
    (if R
       R
       X
    )
  )
)

(defun buildExpr (B N V expr)
  ;Performs a search and replace of a body to make it evaluateable
  (cond
    ((NULL (car B)) nil)
    ((NULL (cdr B)) (append expr (list (matchValue (car B) N V))))
    ((NOT (ATOM (car B))) (append expr (list (buildExpr (car B) N V nil )) (buildExpr (cdr B) N V nil)))
    (t (buildExpr (cdr B) N V (append expr (list (matchValue (car B) N V)))))
    )
)

(defun fl-interp (E P)
  (cond
    ((atom E) E)
    (t
      (let ( (f (car E))  (arg (cdr E)) )
        (cond
          ; handle built-in functions
          ((eq f 'if) (if (fl-interp (car arg) P) (fl-interp (cadr arg) P) (fl-interp (caddr arg) P)))
          ((eq f 'null) (NULL (car arg)))
          ((eq f 'atom) (ATOM (car arg)))
          ((eq f 'eq) (eq (fl-interp (car arg) P) (fl-interp (cdr arg) P)))
          ((eq f 'first)  (car (fl-interp (car arg) P)))
          ((eq f 'rest) (cdr (fl-interp (car arg) P)))
          ((eq f 'cons) (cons (fl-interp (car arg) P) (fl-interp (cadr arg) P)) )
          ((eq f 'equal) (equal (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
          ((eq f 'isnumber) (numberp (fl-interp (car arg) P)))
          ((eq f '+) (+ (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
          ((eq f '-) (- (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
          ((eq f '*) (* (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
          ((eq f '>) (> (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
          ((eq f '<) (< (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
          ((eq f '=) (= (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
          ((eq f 'and) (and (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
          ((eq f 'or) (or (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
          ((eq f 'not) (not (fl-interp (car arg) P)))
          ; Handle user defined functions
          ((atom f)
           (let ((N (getNames P)) (V (getValues E)))
             (fl-interp (buildExpr (getBody P) N V nil) P)
           )
          )
          (t  E)
          )
        )
      )
    )
  )
