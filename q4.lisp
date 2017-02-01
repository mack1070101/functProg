(defun split (L)
    (cond ((NULL L) '(() ()))
          ((NULL (cdr L)) (list (car L) ))
          (t (list (cons (car L) (car (split (cddr L)))) ; Add nested if call to check (car (split (cddr L))) for atomicity
                   (cons (car (cdr L)) (car (cdr (split (cddr L)))))))
    )
)
