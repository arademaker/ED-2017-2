;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

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

;;;;;;;;;;;
;; exercise 2.19 Sanjoy Dasgupta, Christos Papadimitriou, Umesh
;; Vazirani --- Algorithms (2006)

(defun merge-sorted-lists (lists)
  "merge several sorted lists of integers using divide-and-conquer."
  (mergesort-skeleton lists #'merge-lists #'halve-list #'first))

;;;;;;;;;;;
;; counting inverses algorithm from coursera course number of
;; inversions is 2407905288 (is it too slow? 144 seconds)

(defparameter *inversions* 0)

(defun merge-and-count (vector)
  "sort a vector made up of integers and return number of inversions
made."
  (setf *inversions* 0)
  (let ((sorted-vector (mergesort-skeleton vector #'count-and-merge)))
    (values *inversions* sorted-vector)))

(defun count-and-merge (vector1 vector2 &optional (sorted-vector (make-array 100000 :adjustable t :fill-pointer 0)))
  "merge two vectors in sorting order and counts number of inversions
made."
  (let ((len1 (length vector1))
	(len2 (length vector2)))
    (if (or (= len1 0) (= len2 0))
	(concatenate 'vector sorted-vector vector1 vector2)
	(let ((first1 (svref vector1 0))
	      (first2 (svref vector2 0)))
	  (cond ((<= first1 first2)
		 (progn (vector-push-extend first1 sorted-vector)
		 (count-and-merge (subseq vector1 1) vector2
				  sorted-vector)))
		((> first1 first2)
		 (progn (incf *inversions* len1)
			(vector-push-extend first2 sorted-vector)
			(count-and-merge vector1 (subseq vector2 1)
					 sorted-vector))))))))

;;;;;;;;;
;; tests

(mergesort (list 83 934 3 -1 -99 3))
(mergesort (list 43 -525 3 -1 5 10 -1 5 -5))
(mergesort nil)
(mergesort (list 5 5 5 5 5))
(merge-sorted-lists '((1 2 3 4) (-1 -2 -3 -4) (8 9 10 11)))
(merge-sorted-lists '(nil nil nil nil))
(merge-sorted-lists '((1 1 1) (1 1 1) (1 1 1)))
(merge-sorted-lists '((-1 1 11)))
(merge-and-count #(1 1 0 0))
(merge-and-count #(0 0 0))
(merge-and-count #())
(merge-and-count #(0 1 2 3))
(merge-and-count #(4 3 2 0 1))
(merge-and-count #(1 10 5 -1 0 10))
