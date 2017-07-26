;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

;; exercise 2.15 Sanjoy Dasgupta, Christos Papadimitriou, Umesh
;; Vazirani --- Algorithms (2006)
;;
;; Show how to implement a split operation in place. splits a vector
;; in three according to a pivot, the first part is made up of
;; elements less than the pivot, the second is made up of elements
;; equal to the pivot, and the third is made up of elements bigger
;; than the pivot.

(defun split-vector (vector pivot)
  "splits vector in three, the first is composed of elements less than
the pivot, the second is composed by elements equal to the pivot, and
the third is composed of elements greater than the pivot."
  (multiple-value-bind (split-vector start end) (split vector pivot)
    (values (subseq split-vector 0 start)
	    (subseq split-vector start end)
	    (subseq split-vector end))))

(defun split (vector pivot &optional (curr 0) (endix 0) (pivots 0) (passed 0))
  "splits vector in three, according to pivot. returns vector and
indices of start and end of pivot elements."
  (let ((lastix (1- (array-dimension vector 0))))
    (when (> passed lastix)
      (return-from split (values
			  vector (- endix pivots -1) (1+ endix))))
    (let ((current (svref vector curr)))
      (cond ((= current pivot)
	     (split vector
		    pivot (1+ curr) curr (1+ pivots) (1+ passed)))
	    ((> current pivot)
	     (split (rotate-to vector curr lastix)
		    pivot curr endix pivots (1+ passed)))
	    ((< current pivot)
	     (split (rotate-to vector curr 0)
		    pivot (1+ curr) (1+ endix) pivots (1+ passed)))))))

(defun swap (vector ix1 ix2)
  "swaps places of vector's elements at indices ix1 ix2."
  (let ((content1 (svref vector ix1))
	(content2 (svref vector ix2)))
    (setf (svref vector ix1) content2)
    (setf (svref vector ix2) content1)
    vector))

(defun rotate-to (vector ix endix)
  "rotates element of vector at index ix until it reaches index
   endix."
  (cond ((= ix endix)
	 vector)
	((> ix endix)
	 (rotate-to (swap vector ix (1- ix )) (1- ix) endix))
	((< ix endix)
	 (rotate-to (swap vector ix (1+ ix)) (1+ ix) endix))))


;; tests
(split-vector #(1 3 4 3 0 -1 3 8 99) 3)
(split-vector #(1 3 4 3 0 -1 3 8 99) 5)
(split-vector #(1 3 4 3 0 -1 3 8 99) -10) ; weird result, but ok as
					  ; -10 is not on list
(split-vector #(1 3 4 3 0 -1 3 8 99) -1)
(split #(10 3 5 6 2 3 8 4 5 7 4) 5)
