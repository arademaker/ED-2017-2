;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

;;; exercise 2.15 Sanjoy Dasgupta, Christos Papadimitriou, Umesh
;;; Vazirani --- Algorithms (2006)

;; show how to implement a split operation in place. splits a vector
;; in three according to a pivot, the first part is made up of
;; elements less than the pivot, the second is made up of elements
;; equal to the pivot, and the third is made up of elements bigger
;; than the pivot.

(defun split
    (vector pivot &optional (curr 0) (endix 0) (pivots 0) (passed 0))
  "splits vector in three, according to pivot."
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

(defun swap(vector ix1 ix2)
  "swaps places of vector's elements at indices ix1 ix2."
  (let ((content1 (svref vector ix1))
	(content2 (svref vector ix2)))
    (setf (svref vector ix1) content2)
    (setf (svref vector ix2) content1)
    vector))

(defun rotate-to(vector ix endix)
  "rotates element of vector at index ix until it reaches index
endix."
  (cond ((= ix endix)
	 vector)
	((> ix endix)
	 (rotate-to (swap vector ix (1- ix )) (1- ix) endix))
	((< ix endix)
	 (rotate-to (swap vector ix (1+ ix)) (1+ ix) endix))))
