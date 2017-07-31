;; DPV chapter 3 -- graphs

;; test graphs
(defparameter *directed-graph* (copy-tree '((a c)
					    (b a d)
					    (c e f)
					    (d c)
					    (e)
					    (f))))

(defparameter *undirected-graph* (copy-tree '((a b c)
					      (b a d e)
					      (c a d)
					      (d b c)
					      (e b)
					      (f g)
					      (g f))))

;; structures
(defstruct (node
	     (:print-function ;this will make node printing only print
			      ;its #label
	      (lambda (node stream k)
		(identity k)  ;ignoring the second argument k (level)
		(format stream "#N(~A)" (node-label node)))))
  "structure nodes: label, its neighbours labels, and its visited
property."
  (label)
  (adj-labels nil)
  (visited nil))

(defstruct (graph (:print-function
		   (lambda (graph stream k)
		     (identity k)
		     (format stream "#G(~{~A~^ ~})" (graph-nodes graph)))))
  (nodes nil))

;; read functions
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

;; init test graphs
(defvar dg (read-graph *directed-graph*))
(defvar ug (read-graph *undirected-graph*))

;; utility functions
(defun get-node (graph node-label)
  (find node-label (graph-nodes graph)
	:key (lambda (node) (node-label node))))

(defun nodefy (node-label graph)
  (if (node-p node-label)
      node-label
      (get-node graph node-label)))

;; DFS
(defun visit-node (node)
  (setf (node-visited node) t))

(defun df-explore-from-node (graph node &optional
					  (pre-visit #'visit-node)
					  (post-visit #'identity))
  "depth-first exploration from a node."
  (funcall pre-visit node)
  (dolist (adj-label (node-adj-labels node))
    (let ((adj-node (get-node graph adj-label)))
      (when (null (node-visited adj-node))
	(df-explore-from-node graph adj-node pre-visit post-visit)))
    (funcall post-visit node)))

(defun df-explore-from-label (graph node-label
			      &optional (pre-visit #'visit-node)
				(post-visit #'identity))
  (df-explore-from-node (get-node graph node-label)
			pre-visit post-visit))

(defun unvisit-nodes(graph)
  (dolist (node (graph-nodes graph))
    (setf (node-visited node) nil)))

(defun df-explore (graph &optional (pre-visit #'visit-node)
			   (post-visit #'identity))
  (unvisit-nodes graph)
  (dolist (node (graph-nodes graph))
    (when (null (node-visited node))
      (df-explore-from-node graph node pre-visit post-visit))))

;;adjacency
(defun is-adjacent-node (node adj)
  "check if adj is adjacent to node."
  (if (member (node-label adj) (node-adj-labels node))
      t
      nil))
#|
(defun are-adjacent-label (graph node-labels)
  "check if every node in (rest node-labels) is adjacent to (first
node-labels)"
  (let ((nodes (mapcar (lambda (node-label) (nodefy node-label graph))
		       node-labels)))
    (notany #'null (mapcar (lambda (node)
			     (is-adjacent-node (first nodes) node))
			   (rest nodes)))))
|#
(defun vertices-reciprocal-p (node graph) ;does a lot of duplicate work
  "check if node is adjacent to all nodes adj to node."
  (notany #'null (mapcar (lambda (adj-label)
			   (is-adjacent-node (get-node graph adj-label) node))
			 (node-adj-labels node))))

(defun directedp (graph)
  (some #'null (mapcar (lambda (node)
			   (vertices-reciprocal-p node graph))
			 (graph-nodes graph))))

;; connected components
(defparameter *connected-nodes* nil)
(defun connected-components (graph)
  (defparameter *connected-nodes* (make-array (length (graph-nodes graph)) :initial-element 0))
  (let ((explore-calls 0))
    (unvisit-nodes graph)
    (dolist (node (graph-nodes graph))
      (when (null (node-visited node))
	(df-explore-from-node graph node
			      (lambda (node) (connected-component-is graph node explore-calls)))
	(incf explore-calls)))
    *connected-nodes*))

(defun connected-component-is (graph node component-id)
  (setf (svref *connected-nodes* (position node (graph-nodes graph))) component-id)
  (visit-node node))

;; tests
(is-adjacent-node (get-node dg 'a) (get-node dg 'c)) ; t
(is-adjacent-node (get-node ug 'a) (get-node ug 'c)) ; t
(is-adjacent-node (get-node dg 'c) (get-node dg 'd)) ; nil
(is-adjacent-node (get-node ug 'a) (get-node ug 'b)) ; nil
;(are-adjacent-label ug '(a c)) ; t
;(are-adjacent-label ug '(b a d)) ; t
;(are-adjacent-label ug '(b a c)) ; nil
(vertices-reciprocal-p (get-node dg 'A) dg) ; nil
(vertices-reciprocal-p (get-node ug 'A) ug) ; t
(directedp ug) ; nil
(directedp dg) ; t
(connected-components ug) ; #(0 0 0 0 0 1 1)
