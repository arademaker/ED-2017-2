;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

;; heap indexing starts from 0.

;; can make one general function out of heap-swap-up/d

(defstruct (heap (:print-function
		   (lambda (heap stream k)
		     (identity k)
		     (format stream "h~A" (heap-vector heap)))))
  (vector nil)
  (predicate #'>=))

;; validation

(defun validate-heap-size (n &optional (iter 1) (total 1))
  "returns the appropriate size for a complete tree."
  (cond ((= n total)
	  n)
	((> n total)
	  (validate-heap-size n (* iter 2) (+ total (* iter 2))))
	((< n total)
	 total)))

(defun validate-heap (heap)
  "validates a heap."
  (let* ((last-ix (heap-last-ix heap))
	 (last-parent-ix (parent-ix last-ix))
	 (predicate (heap-predicate heap)))
    (loop for parent being the elements of
	 (the array (heap-vector heap))
       for ix from 0
       if (<= ix last-parent-ix)
       do (progn (assert (heap>=
			  (left-child heap ix) parent predicate))
		 (assert (heap>=
			  (right-child heap ix) parent predicate)))))
  heap)

;;;;;;;;;
;;start;;
(defun feed-heap (heap init-values)
  "insert values in heap. values must be list."
  (if (endp init-values)
      heap
      (feed-heap (heap-insert heap (first init-values))
		 (rest init-values))))

(defun start-heap (n &key init-values (predicate #'>=))
  "starts up heap (array) with n slots. if n does not make the tree
   complete, use the nearest integer greater than n that would do so."
  (let* ((n (validate-heap-size n))
	 (heap-vec (make-array n :initial-element nil))
	 (heap (make-heap :vector heap-vec :predicate predicate)))
    (feed-heap heap init-values)))

;;;;;;;;;;;;;
;;auxiliary;;
(defun heap-nth (heap n)
  "gets nth element of heap"
  (svref (heap-vector heap) n))

(defun (setf heap-nth) (value heap ix)
  "defines (setf heap-nth as would be expected."
  (setf (svref (heap-vector heap) ix) value))

(defun find-min (heap)
  "returns first element of array, which is minimum value of array."
  (heap-nth heap 0))

(defun heap-last-ix (heap)
  "returns heap's last ix."
  (1- (array-dimension (heap-vector heap) 0)))

(defun heap-length (heap)
  "number of non-nil elements of array"
  (count-if-not #'null (heap-vector heap)))

(defun heap-nil-ix (heap)
  (position nil (heap-vector heap) :from-end t))

(defun set-heap-end (heap element)
  "sets last node of heap to the given element."
  (setf (heap-nth heap (heap-nil-ix heap)) element)
  heap)

(defun left-child-ix (ix)
  "returns ix of left child of ix."
  (1+ (* 2 ix)))

(defun left-child (heap ix)
  "returns left child of ix."
  (heap-nth heap (left-child-ix ix)))

(defun right-child-ix (ix)
  "returns ix of the right child of ix."
  (* 2 (1+ ix)))

(defun right-child (heap ix)
  "returns the right child of ix."
  (heap-nth heap (right-child-ix ix)))

(defun parent-ix (ix)
  "returns ix of the parent of ix."
  (floor (/ (1- ix) 2)))

(defun get-parent (heap ix)
  "returns the parent of the ix."
  (heap-nth heap (parent-ix ix)))

(defun heap>= (element1 element2 &optional (predicate #'>=))
  "same as predicate but defines what happens when first element is
nil."
  (cond ((null element1)
	 t)
	((null element2)
	 nil)
	(t (funcall predicate element1 element2))))

;;;;;;;;;;;;;;
;;operations;;
(defun heap-insert (heap element)
  "inserts element into heap."
  (let ((nil-ix (heap-nil-ix heap)))
    (heapify-up (set-heap-end heap element) nil-ix)))

(defun heap-swap! (heap ix parent-ix)
  "rotates elements of indexes ix and parent-ix in heap."
  (rotatef (heap-nth heap parent-ix) (heap-nth heap ix))
  heap)

(defun heap-swap-up? (heap ix)
  "should we swap element at ix by its parent?"
  (let ((parent-ix (parent-ix ix))
	(parent (get-parent heap ix))
	(node (heap-nth heap ix))
	(predicate (heap-predicate heap)))
    (if (heap>= parent node predicate)
	(heapify-up (heap-swap! heap ix parent-ix) parent-ix)
	heap)))

(defun heapify-up (heap ix)
  "checks if element at index ix is in correct position relative to
its parent"
  (if (equal ix 0)
      heap
      (heap-swap-up? heap ix)))

(defun smallest-child (heap ix)
  "given index ix returns the index of the smallest child."
  (let ((right (right-child heap ix))
	(left (left-child heap ix)))
    (if (funcall (heap-predicate heap) right left)
	(left-child-ix ix)
	(right-child-ix ix))))

(defun heap-swap-d? (heap child-ix)
  "should we swap element at ix by its parent?"
  (let ((parent-ix (parent-ix child-ix))
	(parent (get-parent heap child-ix))
	(child (heap-nth heap child-ix))
	(predicate (heap-predicate heap)))
    (if (heap>= parent child predicate)
	(heapify-down (heap-swap! heap child-ix parent-ix) child-ix)
	heap)))
  
(defun heapify-down (heap ix)
  "checks if element at index ix is in correct position relative to
its children."
  (if (> (left-child-ix ix) (heap-length heap))
      heap
      (heap-swap-d? heap (smallest-child heap ix))))

(defun heap-delete (heap ix)
  "deletes element at index ix in heap."
  (let ((rm-element (heap-nth heap ix)))
    (setf (heap-nth heap ix) nil)
    (values rm-element
	    (heapify-down heap ix))))

;; examples

(validate-heap-size 5) ; not ok: 7
(validate-heap-size 7) ; 7
(start-heap 7 :init-values '(5 6 3 2 9 10 1)) ; #(1 3 2 6 9 10 5)
(validate-heap (make-heap :vector #(1 3 2 6 9 10 5))) ; #(1 3 2 6 9 10 5)
;;(validate-heap #(1 3 2 6 9 10 1)) ; assertion error
(start-heap 7 :init-values '(5 6 3 2 9 10 1)) ; #(1 3 2 6 9 10 5)
(heap-delete (start-heap 7 :init-values '(5 6 3 2 9 10 1)) 6) ; #(1 3 2 6 9 10 NIL)
(start-heap 7 :init-values '("amanhã" "zebra" "amanda" "bobo" "boçal" "18" "yupi") :predicate #'string>=) ; h#(18 amanda amanhã yupi boçal bobo zebra)
