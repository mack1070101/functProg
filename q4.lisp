(defun split (L)
    (cond ((NULL L) '(() ()))
          ((NULL (cdr L)) (list (car L) '()))
          (t (list (list (car L) (car (split (cddr L))))
                   (list (car (cdr L)) (car (cdr ((cddr L)))))))
                    
    )
)

(t (let ((split-rest (split-list (rest (rest l)))))
     (list (list* (first l) (first split-rest))
           (list* (second l) (second split-rest))
           )))
