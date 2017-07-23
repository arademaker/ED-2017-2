;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

;; ideal implementation would be using arrays.

(defun mergesort-skeleton
    (seq merge-fn &optional (return-fn #'identity))
  "this skeleton is used in mergesort or in merging sorted sequences."
  (if (> (length seq) 1)
      (multiple-value-bind (seq1 seq2) (halve-sequence seq)
	(funcall merge-fn
		 (mergesort-skeleton seq1 merge-fn return-fn)
		 (mergesort-skeleton seq2 merge-fn return-fn)))
      (funcall return-fn seq)))

(defun halve-sequence (seq)
  "halve a sequence into two, if number of elements is odd the first
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
  "sort a list of integers using the mergesort algorithm."
  (mergesort-skeleton a-list #'merge-lists))

;; exercise 2.19 Sanjoy Dasgupta, Christos Papadimitriou, Umesh
;; Vazirani --- Algorithms (2006)

(defun merge-sorted-lists (lists)
  "merge several sorted lists of integers using divide-and-conquer."
  (mergesort-skeleton lists #'merge-lists #'first))

;; counting inverses algorithm
;; from coursera course

(defun merge-and-count (vector)
  "sort a vector made up of integers and return number of inversions
made."
  (defparameter *inversions* 0)
  (let ((sorted-vector (mergesort-skeleton vector #'count-and-merge)))
    (values *inversions* sorted-vector)))

(defun count-and-merge (vector1 vector2)
  "merge two vectors in sorting order and counts number of inversions
made."
  (let ((len1 (length vector1))
	(len2 (length vector2)))
    (if (or (= len1 0) (= len2 0))
	(concatenate 'vector vector1 vector2)
	(let ((first1 (svref vector1 0))
	      (first2 (svref vector2 0)))
	  (cond ((> first1 first2)
		 (progn (incf *inversions* len1)
			(concatenate 'vector (list first2)
			      (count-and-merge vector1
					       (subseq vector2 1)))))
		((<= first1 first2)
		 (concatenate 'vector (list first1)
			      (count-and-merge (subseq vector1 1)
					       vector2))))))))

;; tests

(mergesort (list 83 934 3 -1 -99 3))
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
