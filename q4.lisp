(defun split (L)
    (cond ((NULL L) '(() ()))
          ((NULL (cdr L)) (list L))
          (t (list (cons (car L) (car (split (cddr L))))
                   (cons (car (cdr L)) (car (cdr (split (cddr L)))))))
    )
)
