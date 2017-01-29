(defun mix (X Y)
    (cond ((and (NULL X) (NULL Y)) NIL)
          ((NULL X) Y)
          (t (cons (car X) (mix Y (cdr X))))
    )
)


(defun mix (X Y) ; Alternate
    (cond ((and (NULL X) (NULL Y)) NIL)
          ((NULL X) Y)
          (t (cons (car Y) (mix (cdr Y) X )))
    )
)
