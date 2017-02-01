(defun sumList (x)
    (cond ((NULL x) 0)
          (t (+ (car x) (sumList (cdr x))))
    )
)

(defun subsethelper (S L)
    (let* (
        (saved (subsetsum (- S (car L)) (cdr L)))
    )
    (cond (saved (cons (car L) saved))
          (t (subsetsum S (cdr L)))
    )
    )
)
;add some lines 
(defun subsetsum (S L)
  ; should be sorting, and summing check for correctness
    (cond ((OR (< S 0) (NULL L)) NIL)
          ((> (car(sort (copy-list L) '<)) S) NIL);List cannot contain the subset because smallest > S
          ((< (sumlist L) S) NIL) ; List cannot sum to correct value because it's too small
          ((= S (car L)) (list (car L)))
          (t ( subsethelper S L))
    )
)
