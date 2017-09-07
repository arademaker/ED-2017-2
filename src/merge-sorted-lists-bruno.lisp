;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

;; exercise 2.19 Sanjoy Dasgupta, Christos Papadimitriou, Umesh
;; Vazirani --- Algorithms (2006)

(load "mergesort.lisp"

(defun merge-sorted-lists (lists)
  "merge several sorted lists of integers using divide-and-conquer."
  (mergesort-skeleton lists #'merge-lists #'halve-list #'first))


;; tests
(merge-sorted-lists '((1 2 3 4) (-1 -2 -3 -4) (8 9 10 11)))
(merge-sorted-lists '(nil nil nil nil))
(merge-sorted-lists '((1 1 1) (1 1 1) (1 1 1)))
(merge-sorted-lists '((-1 1 11)))
