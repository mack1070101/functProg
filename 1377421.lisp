#|Question 1

 xmember returns true if an Y is apart of a list, else it returns nil.
 It does this by checking if Y is equal to nil, if it is, the function returns nil.
 It then checks for equality between the first element of X and Y. If it is, true is
 returned. If the equality isn't met, the function recurses deeper until X is a null list

|#
(defun xmember (X Y)
    (cond ((NULL X)  NIL) ; IF X == NIL RETURN NIL
          ((equal (car X) Y) T )   ; IF 1st element of X == Y, return True
          ((xmember (cdr X) Y) T)))  ; Else recurse deeper

#|Question 2

 Flatten first checks to see if a list is null. If it is, NIL is returned.
 Else, the function checks to see if the first element of X is an atom. If it is, the
 append of the first element of X (as a list)  and the result of flatten on the rest of the
 list is returned. If the first element of X isn't an atom, the results of flatten on the first
 element, and the rest of X is returned.

|#
(defun flatten (X)
  ; Remove all nesting from a list
    (cond ((NULL X) NIL) ; If X is NIL return NIL
          ((ATOM (car X)) (append (list (car X)) (flatten (cdr X)))) ; Create a list from X and the flattening of the rest of X
          (t (append (flatten (car X)) (flatten (cdr X)))))) ; Create a list from the first element and the flattening of the rest of X

#|Question 3

 If both X and Y are null, return NIL. Else, cons a list from the first element of Y
 and the result of mix being called on the rest of Y, when it's position is interchanged with X.
 The interchange of X and Y is what results in the interleaving of values

|#
(defun mix (X Y)
  ; Interleave two lists, return null if both lists are null. 
    (cond ((and (NULL X) (NULL Y)) NIL)
          ((NULL X) Y)
          (t (cons (car Y) (mix (cdr Y) X )))))

#|Question 4

 If the list is null, return two nils. Else if L is a single item list, return it a nested list
 for assembly into larger lists at a higher level. Else make a list containing two lists, that are created by
 con of the first item of L and the third item of split L, and a cons of the second item of L and the 4th item
 of split L

|#
(defun split (L)
  ; Split a list into two halves while maintaining nesting
    (cond ((NULL L) '(() ())) ; If L is null, return two null lists
          ((NULL (cdr L)) (list L)) ; If L is a single item, return a list of a single item
          (t (list (cons (car L) (car (split (cddr L)))) ; Construct a list containing two lists of each half
                   (cons (car (cdr L)) (car (cdr (split (cddr L)))))))))

#|Question 5

 * 5.1
 It is not always true that (split (mix L2 L1)) returns the list (L1 L2). By counter example, if the pairs lists are of uneven length, the functions will not return (L1 L2).
    Ex:
    (split (mix '(A) '(B C D E F))) → ((B C E) (A D F))
* 5.2

|#

#|Question 6

 In subsetsum, if the list is empty, or the sum (S) is negative, return nothing, as it is unuseable. Next, 
 sort the list, and if the smallest item of the list L is larger than the sum S, discard L, as it cannot 
 contain a subset that is correct. If the sum of the list is less than S, discard the list as it is 
 doesn't contain enough elements to create the sum. Finally, if the first item of the list is equal to the sum, return the first 
 item of the list as a single item list. If no conditions are true, call subset helper to do the recursion on
 the list. In subsethelper, define a variable saved, as the result of a recursive call of subsetsum on S minus the first 
 element of L and L without the first item. If saved contains a subset, return that subset. Else, call subsetsum on S and L
 without the first item. Finally, sumlist exists to simply sum a list recursively.

|#
(defun subsetsum (S L)
  ; Compute whether a subset exists that sums to S. If it does, return the subset. Else return nil
    (cond ((OR (< S 0) (NULL L)) NIL)
          ((> (car(sort (copy-list L) '<)) S) NIL);List cannot contain the subset because smallest > S
          ((< (sumlist L) S) NIL) ; List cannot sum to correct value because it's too small
          ((= S (car L)) (list (car L)))
          (t ( subsethelper S L))
    )
)

(defun subsethelper (S L)
  ; Perform the recursion of subsetsum
    (let* ( ;Define a variable saved to prevent recomputation of the sublist
        (saved (subsetsum (- S (car L)) (cdr L)))
    )
    (cond (saved (cons (car L) saved)) ; Check whether to return saved, or call subsetsum again
          (t (subsetsum S (cdr L))))))

(defun sumList (x)
  ; Sum a list recursively
    (cond ((NULL x) 0)
          (t (+ (car x) (sumList (cdr x))))))
