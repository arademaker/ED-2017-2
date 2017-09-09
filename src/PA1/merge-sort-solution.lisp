
(defun read-data (filename)
  (with-open-file (in filename)
    (let ((res))
      (do ((line (read-line in nil nil)
		 (read-line in nil nil)))
	  ((null line)
	   (reverse res))
	(push (parse-integer line) res)))))


(defun merge-and-count (list1 list2 &optional (response nil) (counter 0))
  (if (not (and list1 list2))
      (values (append (reverse response) (or list1 list2)) counter)
      (if (<= (car list1) (car list2))
	  (merge-and-count (cdr list1) list2
			   (cons (car list1) response)
			   counter)
	  (merge-and-count list1 (cdr list2)
			   (cons (car list2) response)
			   (+ counter (length list1))))))


(defun sort-and-count (alist)
  (let ((length (length alist)))
    (if (equal length 1)
	(values alist 0)
	(multiple-value-bind (x b)
	    (sort-and-count (subseq alist 0 (round length 2)))
	  (multiple-value-bind (y c)
	      (sort-and-count (subseq alist (round length 2)))
	    (multiple-value-bind (z d)
		(merge-and-count x y)
	      (values z (+ b c d))))))))


; how to execute:
; (sort-and-count (read-data "IntegerArray.txt"))
