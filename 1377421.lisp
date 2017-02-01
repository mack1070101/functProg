;Question 1
; Documentation: xmember returns true if an Y is apart of a list, else it returns nil.
; It does this by checking if Y is equal to nil, if it is, the function returns nil.
; It then checks for equality between the first element of X and Y. If it is, true is
; returned. If the equality isn't met, the function recurses deeper untill X is a null list
(defun xmember (X Y)
    (cond ((NULL X)  NIL) ; IF X == NIL RETURN NIL
          ((equal (car X) Y) T )   ; IF 1st element of X == Y, return True
          ((xmember (cdr X) Y) T)  ; Else recurse deeper
    )
)
;Question 2
; Documentation: Flatten first checks to see if a list is null. If it is, NIL is returned.
; Else, the function checks to see if the first element of X is an atom. If it is, the
; append of the first element of X (as a list)  and the result of flatten on the rest of the
; list is returned. If the first element of X isn't an atom, the results of flatten on the first
; element, and the rest of X is returned.
(defun flatten (X)
    (cond ((NULL X) NIL) ; If X is NIL return NIL
          ((ATOM (car X)) (append (list (car X)) (flatten (cdr X)))) ; Create a list from X and the flattening of the rest of X
          (t (append (flatten (car X)) (flatten (cdr X)))) ; Create a list from the first element and the flattening of the rest of X
    )
)
;Question 3
; Documentation: If both X and Y are null, return NIL. Else, cons a list from the first element of Y
; and the result of mix being called on the rest of Y, when it's position is interchanged with X.
; The interchange of X and Y is what results in the interleaving of values
(defun mix (X Y)
    (cond ((and (NULL X) (NULL Y)) NIL)
          ((NULL X) Y)
          (t (cons (car Y) (mix (cdr Y) X )))
    )
)

;Question 4
; Documentation: If the list is null, return two nils. Else if L is a single item list, return it a nested list
; for assembly into larger lists at a higher level. Else make a list containing two lists, that are created by
; con of the first item of L and the third item of split L, and a cons of the second item of L and the 4th item 
; of split L
(defun split (L)
    (cond ((NULL L) '(() ())) ; If L is null, return two null lists
          ((NULL (cdr L)) (list L)) ; If L is a single item, return a list of a single item
          (t (list (cons (car L) (car (split (cddr L)))) ; Construct a list containing two lists of each half
                   (cons (car (cdr L)) (car (cdr (split (cddr L)))))))
    )
)

;Question 5
; * 5.1
; It is not always true that (split (mix L2 L1)) returns the list (L1 L2). By counter example, if the pairs lists are of uneven length, the functions will not return (L1 L2). 
;    Ex:
;    (split (mix '(A) '(B C D E F))) → ((B C E) (A D F))
;* 5.2

;Question 6

