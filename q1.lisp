(defun xmember (X Y)
    (cond ((NULL X)  NIL) ; IF X == NIL RETURN NIL
          ((equal (car X) Y) T )   ; IF 1st element of X == Y, return True
          ((xmember (cdr X) Y) T)  ; Else recurse deeper
    )
)

