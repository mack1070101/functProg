(defun split (L)
    (cond ((NULL L) '(() ()))
          ((NULL (cdr L)) (list (car L) '()))
          (t (list (cons (car L) (car (split (cddr L))))
                   (cons (car (cdr L)) (car (cdr (split (cddr L)))))))
                    
    )
)

(t (let ((split-rest (split-list (rest (rest l)))))
     (list (list* (first l) (first split-rest))
           (list* (second l) (second split-rest))
           )))
