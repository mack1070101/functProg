(defun sumList (x)
    (cond ((NULL x) 0)
          (t (+ (car x) (sumList (cdr x))))
    )
)

(defun subsetsum (S L)
  ; should be sorting, and summing check for correctness
    (cond ((OR (< S 0) (NULL L)) NIL)
          ((> (car(sort (copy-list L) '<)) S) NIL);List cannot contain the subset because smallest > S
          ((< (sumlist L) S) NIL) ; List cannot sum to correct value because it's too small
          ((= S (car L)) (list (car L)))
          ((subsetsum (- S (car L)) (cdr L)) (cons (car L) (subsetsum (- S (car L)) (cdr L)))) ; Add let statment to improve computation
          (t (subsetsum S (cdr L)))
    )
)

; Cannot create a let conditon inside a cond → create a wrapper of the bottom two lines
