(defun pruneList (S L)
    (cond ((NULL L) L)  ; If empty return the empty list
          ((and (NULL (cdr L)) (< (car L) S)) (list (car L))) ; If it is a single iteme list, and the item is less than S, return it
          ((< (car L) S) (append (list (car L)) (pruneList S (cdr L)))) ; Build a list of items that are less than sum 
    )
)

(defun subsetsum (S L)
    (cond ((= s 0) L) ; If s = 0, return the list
          ((AND (NULL L) (not (= S 0))) nil) ; If the list is empty, and sum != 0, return nil
          (t ((subsetsum)))
    )
)
