(defun strip (X)
    (cond ((atom X) (LIST X))
          ((NULL (cdr X)) (strip (car X)))
          (t (cons  (car X) (strip (cdr X))))
    ) 
)



(print (flatten '(a (b c) d)))


(defun flatten(X)
    (cond ((NULL (cdr (strip X))) (strip X)) ;If a de nested X is a single item list, return it
          (t (append (list (car X)) (flatten (cdr X))))
    )
)


(defun flatten (X)
    (cond ((ATOM X) (list X))
          ((NULL (cdr X) ) (car X))
          ((ATOM (car X)) (append (list (car X)) (flatten (cdr X))))
          (t (append (car X) (flatten (cdr X))))
    )
)
