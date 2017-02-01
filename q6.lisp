(defun sumList (x)
    (cond ((NULL x) 0)
          (t (+ (car x) (sumList (cdr x))))
    )
)
(defun lastx (x)
    (cond ((NULL (cdr X)) (car x))
          (t (last (cdr x)))
    )
)

(defun listwithoutlast (x)
    (cond ((NULL (cdr x)) nil)
          (t(append (list (car x)) (listwithoutlast (cdr x))))
    )
)

(defun subsetsum (S L)
  ; should be sorting, and summing check for correctness
    (cond ((OR (< S 0) (NULL L)) NIL)
          ((> (car(sort (copy-list L) '<)) S) NIL);List cannot contain the subset because smallest > S
          ((< (sumlist L) S) NIL) ; List cannot sum to correct value because it's too small
          ((= S (car L)) (list (car L)))
          (t (append (subsetsum S (cdr L)) (subsetsum (- S (car L)) (cdr L))))
    )
)


;first bc checks for equality between 1st element of 1st element of sorted listkkkkk
; check if sum of list is < L = null
; Check if sorted list has 1st element > sum = null


((> (CAR (SORT '(1 48 18 21) '<)) 4))
