(defun split (L)
    (cond ((NULL L) '(() ()))
          ((NULL (cdr L)) (list (car L) '()))
          (t (list (append (list(car L)) (list(car (split (cddr L)))))
                   (cons (car (cdr L)) (car (cdr (split (cddr L)))))))
                    
    )
)
