;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

(defun halve-sequence (seq)
  "halve a sequence into two, if number of elements is odd the first
   list is longer."
  (let ((len (length seq)))
    (values (subseq seq 0 (ceiling (/ len 2)))
	    (subseq seq (ceiling (/ len 2))))))

;; why is this not more efficient than halve-sequence (using time)?
(defun halve-list (list &optional (count 0) (list1 nil) (list2 nil))
  "halve list in two."
  (if (endp list)
      (values list1 list2)
      (cond ((evenp count)
	     (halve-list (rest list) (1+ count)
			 (cons (first list) list1) list2))
	    ((oddp count)
	     (halve-list (rest list) (1+ count)
			 list1 (cons (first list) list2))))))

(defun mergesort-skeleton
    (seq merge-fn &optional (split-fn #'halve-sequence)
		    (return-fn #'identity))
  "this skeleton is used in mergesort or in merging sorted sequences."
  (if (> (length seq) 1)
      (multiple-value-bind (seq1 seq2) (funcall split-fn seq)
	(funcall merge-fn
		 (mergesort-skeleton seq1 merge-fn split-fn return-fn)
		 (mergesort-skeleton seq2 merge-fn split-fn return-fn)))
      (funcall return-fn seq)))

;; vanilla mergesort algorithm
;; ideal implementation would be using arrays.
(defun merge-lists (list1 list2 &optional (sorted-list nil))
  "merge lists according to size of first argument."
  (if (or (endp list1) (endp list2))
      (append (reverse sorted-list) list1 list2)
      (let ((car1 (first list1))
	    (car2 (first list2)))
	(cond ((<= car1 car2)
	       (merge-lists (rest list1) list2 (cons car1 sorted-list)))
	      ((> car1 car2) ;being exhaustive so errors are caught
	       (merge-lists list1 (rest list2) (cons car2 sorted-list)))))))

(defun mergesort (a-list)
  "sort a list of integers using the mergesort algorithm."
  (mergesort-skeleton a-list #'merge-lists #'halve-list))

;;
;; tests
(mergesort (list 83 934 3 -1 -99 3))
(mergesort (list 43 -525 3 -1 5 10 -1 5 -5))
(mergesort nil)
(mergesort (list 5 5 5 5 5))
