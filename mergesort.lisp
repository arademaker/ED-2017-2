;;ideal implementation would be using arrays, right?
(defun mergesort(a-list)
  ""
  (if (> (length a-list) 1)
      (multiple-value-bind (list1 list2) (halve-list a-list)
	(merge-lists (mergesort list1) (mergesort list2)))
      a-list))

(defun halve-list(a-list)
  "halve list into two, if number of elements is odd the first list is
longer."
  (let ((len (length a-list)))
    (values (subseq a-list 0 (round (/ len 2))) (subseq
    a-list (round (/ len 2))))))

(defun merge-lists(list1 list2)
  "merge lists according to size of first argument."
  (let ((car1 (first list1))
	(car2 (first list2))
	(cdr1 (rest list1))
	(cdr2 (rest list2)))
    (cond ((endp list1) list2)
	  ((endp list2) list1)
	  ((<= car1 car2) (cons car1 (merge-lists cdr1 list2)))
	  (t (cons car2 (merge-lists list1 cdr2))))))
