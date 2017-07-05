

(let ((a (make-array 100000000 :initial-element 1)))
  (aref a 10000))

(let ((a (loop for x from 0 to 1000000 collect 1)))
	   (dolist (v (list 10 100 1000 10000 100000))
	     (time (nth v a))))


(defun mynth-itr (k alist)
  "See http://www.gigamonkeys.com/book/loop-for-black-belts.html"
  (loop for tmp = alist then (cdr tmp)
	for v from (- k 1) downto 0
	finally (return (car tmp))))

(defun mynth-rec (k alist)
  (if (equal k 0)
      (car alist)
      (mynth-rec (- k 1) (cdr alist))))




