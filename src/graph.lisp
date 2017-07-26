;; DPV chapter 3 -- graphs


(defparameter *test-graph* (copy-tree '((a c)
					(b a d)
					(c e f)
					(d c)
					(e)
					(f))))

(defstruct (node
	     (:print-function ;this will make node printing only print
			      ;its #label
	      (lambda (node stream k)
		(identity k)  ;ignoring the second argument k (level)
		(format stream "#~A" (node-label node)))))
  "structure nodes: label, its neighbours labels, and its visited
property."
  (label)
  (adj-labels nil)
  (visited nil))

(defstruct (graph (:print-function
		   (lambda (graph stream k)
		     (identity k)
		     (format stream "#(~{~A~^ ~})" (graph-nodes graph)))))
  (nodes nil))

(defun read-node (node-list)
  "get unique nodes in graph. in a graph-list, there is one sub-list
for each node, and its car is the node, and its cdr are the nodes
adjacent to it."
  (let ((node-label (first node-list))
	(adj-labels (rest node-list)))
    (make-node :label node-label :adj-labels adj-labels :visited nil)))

(defun make-nodes (graph-list)
  (mapcar #'read-node graph-list))

(defun read-graph (graph-list)
  "read graph-symbol and create the graph and node structs referenced
  in it."
  (let ((nodes (make-nodes graph-list)))
    (make-graph :nodes nodes)))

(defun get-node (graph node-label)
  (find node-label (graph-nodes graph)
	:key (lambda (node) (node-label node))))

(defun visit-node (node)
  (setf (node-visited node) t))

(defun df-explore-from-node (graph node &optional (pre-visit #'visit-node) (post-visit #'identity))
  "depth-first exploration from a node."
  (funcall pre-visit node)
  (dolist (adj-label (node-adj-labels node))
    (let ((adj-node (get-node graph adj-label)))
      (when (null (node-visited adj-node))
	(df-explore-from-node graph adj-node pre-visit post-visit)))
    (funcall post-visit node)))

(defun df-explore-from-label (graph node-label &optional (pre-visit #'visit-node) (post-visit #'identity))
  (df-explore-from-node (get-node graph node-label) pre-visit post-visit))

(defun df-explore (graph &optional (pre-visit #'visit-node) (post-visit #'identity))
  (dolist (node (graph-nodes graph))
    (when (null (node-visited node))
      (df-explore-from graph node pre-visit post-visit))))
  
	
	
  
