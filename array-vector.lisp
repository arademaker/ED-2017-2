

(let ((a (make-array 100000000 :initial-element 1)))
  (aref a 10000))


(let ((a (loop for x from 0 to 1000000 collect 1)))
	   (dolist (v (list 10 100 1000 10000 100000))
	     (time (nth v a))))

(defun mynth (k alist)
  (let ((tmp alist))
    (dotimes (v k (car tmp))
      (setf tmp (cdr tmp)))))


(defun mynth (k alist)
  (if (equal k 0)
      (car alist)
      (mynth (- k 1) (cdr alist))))




