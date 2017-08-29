;;; exercise 2.20 Sanjoy Dasgupta, Christos Papadimitriou, Umesh
;;; Vazirani --- Algorithms (2006)
;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

;; takes an array of integers and sorts them. the process is done in
;; three steps:
;; 1. count the numbers of each integer;
;; 2. count how many elements are less than each integer;
;; 3. fill up sorted vector with this info.

(defun counting-sort(vector min max)
  "takes a vector of integers, its size min and max values, and
   returns it sorted using the counting sort algorithm."
  (let* ((empty-count-vector (make-array (- max min -1) :initial-element 0))
	 (count-vector (count-ints vector empty-count-vector min))
	 (accu-vector (accumulate-ints count-vector))
	 (size (length vector))
	 (empty-sorted-vector (make-array size :initial-element 0)))
    (sort-count accu-vector vector empty-sorted-vector min (1- size))))


(defun count-ints(vector count-vector min &optional (ix 0))
  "vector is a vector of integers, count-vector is (initially) a zero
   vector, min is the smallerinteger and ix is the current number
   being counted. return count-vector where count-vector[ix] is number
   of elements equal to min+ix in vector."
  (if (= ix (length vector))
      count-vector
      (progn (incf (svref count-vector (- (svref vector ix) min)))
	     (count-ints vector count-vector min (1+ ix)))))


(defun accumulate-ints(count-vector &optional (ix 0))
  "turns a count-vector into an accumulated-count-vector, where
   accumulated-count-vector[ix] is the number of elements less than or
   equal to min+ix in original vector."
  (cond ((= ix (length count-vector))
	 count-vector)
	((> ix 0)
	 (incf (svref count-vector ix) (svref count-vector (1- ix)))
	 (accumulate-ints count-vector (1+ ix)))
	((= ix 0)
	 (accumulate-ints count-vector (1+ ix)))))


(defun get-count(count-vector integer min)
  "gets count or accumulated count of integer from count-vector."
  (svref count-vector (- integer min)))


(defun sort-count(count-vector vector sorted-vector min ix)
  "for each element of vector, checks at count-vector how many
   elements are less or equal than it, and places it at the
   appropriate index at sorted-vector. decrements this count in
   count-vector, and goes to the next element."
  (if (< ix 0)
      sorted-vector
      (let ((current (svref vector ix)))
	(setf (svref sorted-vector (1- (get-count count-vector current min))) current)
	(decf (svref count-vector (- current min)))
	(sort-count count-vector vector sorted-vector min (1- ix)))))

;; tests

(counting-sort #(0 0 0 0 0) 0 0)
(counting-sort #(1 0 5 0 -1 10 -2 5 0) -2 10)
(counting-sort #(-2 10 -2 10 -2 10 10 -2 -2) -2 10)
