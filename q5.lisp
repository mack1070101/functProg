* 5.1
It is not always true that (split (mix L2 L1)) returns the list (L1 L2). By counter example, if the pairs lists are of uneven length, the functions will not return (L1 L2). 
    Ex:
    (split (mix '(A) '(B C D E F))) → ((B C E) (A D F))
* 5.2
Must diagnose 4 first
