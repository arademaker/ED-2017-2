
(defparameter *graph-as-list* '((a b c d)
				(b a e f)
				(c a f)
				(d g h)
				(e b i j)
				(f b c)
				(g d h)
				(h d g)
				(i e j)
				(j e i)
				(k l)
				(l k)))


(defclass graph ()
  ((nodes :initform nil :initarg :nodes)))

(defclass node ()
  ((name     :initarg :name)
   (visited  :initform nil :initarg :visited)
   (neibs    :initform nil :initarg :neibs))) 

(defun find-node (graph s)
  (find s (slot-value graph 'nodes) :test (lambda (a n) (equal a (slot-value n 'name)))))


(defun make-graph (graph)
  (let ((g (make-instance 'graph)))
    (dolist (node (mapcar #'car graph))
      (push (make-instance 'node :name node)
	    (slot-value g 'nodes)))
    (dolist (alist graph)
      (let ((v     (find-node g (car alist)))
	    (neibs (cdr alist)))
	(dolist (lu neibs)
	  (let ((u (find-node g lu)))
	    (push u (slot-value v 'neibs))))))
    g))





(defun dfs (graph)
  ...)

(defun dfs (graph)
  ....)

