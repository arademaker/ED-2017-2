;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.
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
		(format stream "#n.~A" (node-label node)))))
  "structure nodes: label, its neighbours labels, and its visited
property."
  (label)
  (adj-labels nil)
  (visited nil)
  (connected-component nil)
  (pre nil)
  (post nil))

(defstruct (graph (:print-function
		   (lambda (graph stream k)
		     (identity k)
		     (format stream "#g(~{~A~^ ~})" (graph-nodes graph)))))
  (nodes nil))

;; read functions
(defun read-node (node-list)
  "get unique nodes in graph. in a graph-list, there is one sub-list
for each node, and its car is the node, and its cdr are the nodes
adjacent to it."
  (let ((node-label (first node-list))
	(adj-labels (rest node-list)))
    (make-node :label node-label :adj-labels adj-labels)))

(defun make-nodes (graph-list)
  (mapcar #'read-node graph-list))

(defun read-graph (graph-list)
  "read graph-symbol and create the graph and node structs referenced
  in it."
  (let ((nodes (make-nodes graph-list)))
    (make-graph :nodes nodes)))

;; init test graphs
(defparameter dg (read-graph *directed-graph*))
(defvar ug (read-graph *undirected-graph*))

;; utility functions
(defun get-node (graph node-label)
  (find node-label (graph-nodes graph)
	:key (lambda (node) (node-label node))))

(defun nodefy (node-label graph)
  (if (node-p node-label)
      node-label
      (get-node graph node-label)))

(defun cons-if (element sequence predicate)
  (if (funcall predicate element)
      (cons element sequence)
      sequence))

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
	(df-explore-from-node graph adj-node pre-visit post-visit))))
  (funcall post-visit node))

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
  "df explore the graph."
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
(defun set-connected-component (node component-id)
  (setf (node-connected-component node) component-id))

(defun set-connected-component-and-visit (node component-id)
  (set-connected-component node component-id)
  (visit-node node))

(defun gather-component (graph-nodes ccid &optional (cc nil))
  "get all nodes which are part of connected component ccid."
  (if (endp graph-nodes)
      cc
      (gather-component (rest graph-nodes) ccid (cons-if (first graph-nodes) cc (lambda (node) (= (node-connected-component node) ccid))))))
    

(defun show-connected-components (graph ccid &optional (connected-components nil))
  (if (< ccid 0)
      connected-components
      (show-connected-components graph (1- ccid) (cons (gather-component (graph-nodes graph) ccid) connected-components))))
  

(defun connected-components (graph)
  "assign ccids to each node in graph."
  (let ((explore-calls 0))
    (unvisit-nodes graph) ;i don't seem able to reuse df-explore here
    (dolist (node (graph-nodes graph))
      (when (null (node-visited node))
	(df-explore-from-node graph node
			      (lambda (node) (set-connected-component-and-visit node explore-calls)))
	(incf explore-calls)))
  (show-connected-components graph (1- explore-calls))))

;; dags
(defvar *clock* 1)

(defun previsit (node)
  (setf (node-pre node) *clock*)
  (incf *clock*)
  (visit-node node))

(defun postvisit (node)
  (setf (node-post node) *clock*)
  (incf *clock*))

(defun reset-node-pre-post (node)
  (setf (node-pre node) nil)
  (setf (node-post node) nil))

(defun reset-nodes-pre-post (graph)
  (dolist (node (graph-nodes graph))
    (reset-node-pre-post node)))

(defun show-pre-post (graph-nodes &optional (pre-post-list nil))
  (if (endp graph-nodes)
      pre-post-list
      (let ((node (first graph-nodes)))
	(show-pre-post (rest graph-nodes) (acons node (list (node-pre node) (node-post node)) pre-post-list))))) 

(defun visit-nodes (graph)
  "mark pre and post numbers in df exploration."
  (setf *clock* 1)
  (reset-nodes-pre-post graph)
  (df-explore graph #'previsit #'postvisit)
  (show-pre-post (graph-nodes graph)))

(defun back-edge? (node adj)
  (let ((pre-node (node-pre node))
	(post-node (node-post node))
	(pre-adj (node-pre adj))
	(post-adj (node-post adj)))
	(when (and (< pre-adj pre-node) (> post-adj post-node))
	  t)))

(defun any-back-edges? (graph)
  (let ((nodes (graph-nodes graph)))
    (dolist (node nodes)
      (dolist (adj (node-adj-labels node))
	(when (back-edge? node (get-node graph adj))
	  (return-from any-back-edges? t))))))

(defun is-dag? (graph)
  (visit-nodes graph)
  (not (any-back-edges? graph)))

(defun linearize-dag (graph)
  (visit-nodes graph)
  (sort (graph-nodes graph) #'> :key #'node-post))

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
(connected-components ug) ; ((#N(E) #N(D) #N(C) #N(B) #N(A)) (#N(G) #N(F)))
(is-dag? dg) ; t
(is-dag? ug) ; nil
