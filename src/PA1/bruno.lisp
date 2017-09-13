;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

(load #p"../mergesort-bruno.lisp")

;;;;;;;;;;;
;; counting inverses algorithm from coursera course number of
;; inversions is 2407905288
(let ((count 0))
  (defun reset-counter (&optional (reset-value 0))
    (setf count reset-value))
  (defun counter (&optional (increment 1))
    (incf count increment)))

(defun merge-and-count (vector)
  "sort a vector made up of integers and return number of inversions
made."
  (reset-counter)
  (let ((sorted-vector (mergesort-skeleton vector #'count-and-merge)))
    (values (counter 0) sorted-vector)))

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
		 (progn (counter len1)
			(vector-push-extend first2 sorted-vector)
			(count-and-merge vector1 (subseq vector2 1)
					 sorted-vector))))))))

;;;;;;;;;
;; tests
(merge-and-count #(1 1 0 0))
(merge-and-count #(0 0 0))
(merge-and-count #())
(merge-and-count #(0 1 2 3))
(merge-and-count #(4 3 2 0 1))
(merge-and-count #(1 10 5 -1 0 10))
