(defun flatten (X)
    (cond ((NULL X) NIL)
          ((ATOM (car X)) (append (list (car X)) (flatten (cdr X))))
          (t (append (flatten (car X)) (flatten (cdr X))))
    )
)

