;Name: Jejoon Ryu
;Student number: 1619980
;Course: CMPUT 325
;Section number: B1


;QUESTION 1
;The function xcount returns the number of atoms appearing in a possibly
;nested list L.
;   Note: NIL is treated as an empty list and not counted as an atom.

(defun xcount (L)
  (cond
    ((null (car L)) (if (null (cdr L))
                        0
                        (xcount (cdr L))))
    ((atom (car L)) (+ 1 (xcount (cdr L))))
    (T (xcount (append (car L) (cdr L))))))


;QUESTION 2
;This function takes in a list with sublists nested to any depth and returns
;a list of atoms such that all the atoms appearing in the given list also appear
;in the resulting list and in the same order.
;   Assumption: NIL and () will not appear in the given list

(defun flatten (x)
  (cond
    ((null x) nil)
    ((atom (car x)) (cons (car x) (flatten (cdr x))))
    (T (flatten (append (car x) (cdr x))))))


;QUESTION 3
;This function takes in a list of atoms and removes the repeated ones.
;The order of the atoms is preserved.

(defun remove-duplicate (x)
  (if (null x)
      nil
      (cons (car x) (remove-duplicate (remove-all (car x) x)))))


;This function returns a list x without any occurrences of an atom a.
;
;   Example: (remove-all 'a '(a a b c)) -> (b c)

(defun remove-all (a x)
  (cond
    ((null x) nil)
    ((equal a (car x)) (remove-all a (cdr x)))
    (T (cons (car x) (remove-all a (cdr x))))))


;QUESTION 4a
;This function mixes the atoms of two lists, L1 and L2, into a single list,
;by choosing elements from L1 and L2 alternatingly.
;If one list is shorter than the another, then it appends all remaining atoms
;from the longer list at the end.

(defun mix (L1 L2)
  (cond
    ((and (null L1) (null L2)) nil)
    ((and (not (null L1)) (null L2)) L1)
    ((and (null L1) (not (null L2))) L2)
    (T (cons (car L1) (mix L2 (cdr L1))))))


;QUESTION 4b
;This function returns a list of two sublists, where the first one is a list of
;atoms in L at odd positions and the second is the list of atoms in L at even positions.
;If L is empty, then a list of two empty list is returned.

(defun split (L)
  (list (split-odd L) (split-even L)))


;This function takes in a list L and returns a list of atoms in L at odd positions.
(defun split-odd (L)
  (cond
    ((null L) nil)
    (T (cons (car L) (split-odd (cddr L))))))


;This function takes in a list L and returns a list of atoms in L at even positions.
(defun split-even (L)
  (cond
    ((null L) nil)
    ((null (cdr L)) nil)
    (T (cons (cadr L) (split-even (cddr L))))))


;QUESTION 5
;This function returns a list of all subsets of L.
(defun allsubsets (L)
  (gen-subsets '(()) L))


;This function accumulates all subsets of L in AC.
;AC must be given with a list of an empty list.
;L can be of any length.
(defun gen-subsets (AC L)
  (cond
    ((null L) AC)
    (T (gen-subsets (append AC (gen-subsets-helper (list (list (car L))) (cdr L)))
                    (cdr L)))))


;This function takes in two lists:
;    AC1: an accumulator such that it has only one sublist
;      L: a list of atoms
;It returns a list such that it has unique combinations of the
;one atom in the sublist of AC1 and the atoms in L.
;
;   Example:
;       (gen-subsets-helper '((a)) '(b c)) -> ((a) (a b) (a c) (a b c))

(defun gen-subsets-helper (AC1 L1)
  (if (null L1)
      AC1
      (gen-subsets-helper (append AC1 (append-to-sublists AC1 (car L1)))
                          (cdr L1))))


;This function takes in a list of lists, called L, and an atom, called a, and
;returns a list such that a is appended to the sublists of L.
;   Example:
;       (append-to-sublists '((a) (b) (c)) 'd) -> ((a d) (b d) (c d))

(defun append-to-sublists (L a)
  (if (null L)
      nil
      (cons (append (car L) (list a))
            (append-to-sublists (cdr L) a))))


;QUESTION 6a
;This function takes two arguments:
;   x: a web page (atom)
;   L: a list of pairs representing linkage
;
;It then returns a list of all web pages that can be
;reached from x, and x is not in the result.
;The order of the web pages is not important.

(defun reached (x L)
  (remove-duplicate (cdr (reached-helper (list x) (remove-linkages x L)))))


;This function takes two arguments:
;    AC: a list that accumulates the reachable web pages
;        Must pass in a list with a source website, x.
;     L: a list of linkages
;
;It returns a list of all web pages that are reachable from x.

(defun reached-helper (AC L)
  (if (null AC)
      nil
      (cons (car AC)
            (reached-helper (append (cdr AC) (reachable-links (car AC) L))
                            L))))


;This function takes in a list of pairs of linkages and a web page x, and
;returns a list of pairs without the pairs whose second value is equal to x.
;
;    Example:
;        (remove-linkages 'a '((a b) (b c) (c a) (a a))) -> ((a b) (b c))

(defun remove-linkages (x L)
  (let ((dest (cadar L)))
    (cond
      ((null L) nil)
      ((equal x dest) (remove-linkages x (cdr L)))
      (T (cons (car L) (remove-linkages x (cdr L)))))))


;This function takes in 2 arguments:
;    link: a target web page (atom)
;       L: a list of pairs representing linkages
;
;It returns a list of destination web pages such that the corresponding
;pair has 'link' as the first value.
;
;The elements in the resulting list can occur more than once if there is
;more than one same pairs in L.
;
;    Example:
;        (reachable-links 'a '((a b) (a b) (b c) (c d) (a e))) -> (b b e)

(defun reachable-links (link L)
  (let ((source (caar L)) (dest (cadar L)))
    (cond
      ((null L) nil)
      ((equal link source) (cons dest (reachable-links link (cdr L))))
      (T (reachable-links link (cdr L))))))


;QUESTION 6b
;This function takes in 2 arguments:
;    S: a list of web pages
;    L: a list of linkage pairs
;
;It returns a permutation of S such that the web pages are ordered based on
;the number of direct links from other web pages.
;
;Note:
;    - Multiple identical pairs count as one
;    - Self-referring pair does not count

(defun rank (S L)
  (take-first-elements-only (rank-sort (rank-helper (rank-init-AC S) S (remove-duplicate (filter-linkages L))))))


;This function removes self-referring linkages from L.

(defun filter-linkages (L)
  (let ((source (caar L)) (dest (cadar L)))
    (cond
      ((null L) nil)
      ((equal source dest) (filter-linkages (cdr L)))
      (T (cons (car L) (filter-linkages (cdr L)))))))


;This function returns a list such that the elements are the first
;elements of the linkage pairs in L.

(defun take-first-elements-only (L)
  (mapcar #'(lambda (x) (car x)) L))


;This function sorts L in descending order based on the second elements
;of the linkage pairs.

(defun rank-sort (L)
  (sort L (lambda (L1 L2) (> (cadr L1) (cadr L2)))))


;This function returns an accumulator list of pairs such that the first
;element is the web page and the second value is the number of total
;direct links in L.
;The resulting pairs are not ordered in any ways.

(defun rank-helper (AC S L)
  (let ((reached-links (reachable-links (car S) L)))
    (cond
      ((null S) AC)
      (T (rank-helper (rank-update-AC reached-links AC) (cdr S) L)))))


;This function initializes and returns an accumulator for RANK.
;Given a list of web pages, it returns a list of pairs such that the first element is
;the web page and the second value is 0.
;
;    Example:
;        (rank-init-AC '(A B C)) -> ((A 0) (B 0) (C 0))

(defun rank-init-AC (S)
  (cond
    ((null S) nil)
    (T (cons (list (car S) 0) (rank-init-AC (cdr S))))))


;This function receives a list of reached web pages and returns the
;updated accumulator.
;For each web page in reached-L, it increments the second element of
;the corresponding pair in the accumulator by 1.
;    Example:
;        (rank-update-AC '(A B C) '((A 0) (B 1) (C 2) (D 3))) -> ((A 1) (B 2) (C 3) (D 3))

(defun rank-update-AC (reached-L AC)
  (cond
    ((null reached-L) AC)
    (T (rank-update-AC (cdr reached-L) (rank-update-AC-helper (car reached-L) AC)))))


;This function receives a web page and an accumulator and returns back
;the accumulator after updating it.

(defun rank-update-AC-helper (l AC)
  (let ((page (caar AC)) (page-i (cadar AC)))
    (cond
      ((null AC) nil)
      ((equal l page) (cons (list l (+ 1 page-i))
                            (rank-update-AC-helper l (cdr AC))))
      (T (cons (car AC) (rank-update-AC-helper l (cdr AC)))))))


