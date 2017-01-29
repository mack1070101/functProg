(defun mix (X Y) ; Alternate
    (cond ((and (NULL X) (NULL Y)) NIL)
          ((NULL X) Y)
          (t (cons (car Y) (mix (cdr Y) X )))
    )
)
