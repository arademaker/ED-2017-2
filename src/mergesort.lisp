;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

;; ideal implementation would be using arrays.

(defun mergesort-skeleton (a-list a-function &optional (return-function #'identity))
  "this skeleton is used in mergesort or in merging sorted lists."
  (if (> (length a-list) 1)
      (multiple-value-bind (list1 list2) (halve-sequence a-list)
	(funcall a-function
		 (mergesort-skeleton list1 a-function return-function)
		 (mergesort-skeleton list2 a-function return-function)))
      (funcall return-function a-list)))

(defun halve-sequence (seq)
  "halves a sequence into two, if number of elements is odd the first
   list is longer."
  (let ((len (length seq)))
    (values (subseq seq 0 (ceiling (/ len 2)))
	    (subseq seq (ceiling (/ len 2))))))

(defun merge-lists (list1 list2)
  "merge lists according to size of first argument."
  (let ((car1 (first list1))
	(car2 (first list2)))
    (cond ((endp list1) list2)
	  ((endp list2) list1)
	  ((<= car1 car2) (cons car1 (merge-lists (rest list1) list2)))
	  (t (cons car2 (merge-lists list1 (rest list2)))))))

(defun mergesort (a-list)
  "sorts a list of integers using the mergesort algorithm."
  (mergesort-skeleton a-list #'merge-lists))

;; exercise 2.19 Sanjoy Dasgupta, Christos Papadimitriou, Umesh
;; Vazirani --- Algorithms (2006)

(defun merge-sorted-lists (lists)
  "merges several sorted lists of integers using divide-and-conquer."
  (mergesort-skeleton lists #'merge-lists #'first))

;; tests

(mergesort (list 83 934 3 -1 -99 3))
(mergesort nil)
(mergesort (list 5 5 5 5 5))
(merge-sorted-lists '((1 2 3 4) (-1 -2 -3 -4) (8 9 10 11)))
(merge-sorted-lists '(nil nil nil nil))
(merge-sorted-lists '((1 1 1) (1 1 1) (1 1 1)))
(merge-sorted-lists '((-1 1 11)))
