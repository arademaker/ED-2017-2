;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.
;; DPV chapter 3 -- graphs

;; test graphs
(defparameter *directed-graph* (copy-tree '((a c)
					    (b a d)
					    (c e f)
					    (d c)
					    (e f)
					    (f))))

(defparameter *dag-graph* (copy-tree '((a c)
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
  (adj-nodes nil)
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
  (let ((node-label (first node-list)))
    (make-node :label node-label)))

(defun make-nodes (graph-list)
  (mapcar #'read-node graph-list))

(defun get-node (graph node-label)
  (find node-label (graph-nodes graph)
	:key (lambda (node) (node-label node))))

(defun add-adj-nodes (graph node-list)
  (let* ((node-label (first node-list))
	 (node (get-node graph node-label))
	 (adj-labels (rest node-list)))
    (dolist (adj-label adj-labels)
      (push (get-node graph adj-label) (node-adj-nodes node)))))

(defun add-neighbours (graph graph-list)
  (mapcar (lambda (node-list) (add-adj-nodes graph node-list))
	  graph-list))

(defun read-graph (graph-list)
  "read graph-symbol and create the graph and node structs referenced
  in it."
  (let* ((nodes (make-nodes graph-list))
	(graph (make-graph :nodes nodes)))
    (add-neighbours graph graph-list)
    graph))

;; init test graphs
(defvar dg (read-graph *directed-graph*))
(defvar dag (read-graph *dag-graph*))
(defvar ug (read-graph *undirected-graph*))

;; utility functions
(defun cons-if (predicate element sequence)
  (if (funcall predicate element)
      (cons element sequence)
      sequence))

(defun nodefy (node-label graph)
  (if (node-p node-label)
      node-label
      (get-node graph node-label)))

;; DFS
(defun visit-node (node)
  (setf (node-visited node) t))

(defun df-explore-from-node (node &key (pre-visit #'visit-node)
					  (post-visit #'identity))
  (funcall pre-visit node)
  (dolist (adj-node (node-adj-nodes node))
    (unless (node-visited adj-node)
      (df-explore-from-node adj-node :pre-visit pre-visit
			    :post-visit post-visit)))
  (funcall post-visit node))

(defun df-explore-from-label (graph node-label
			      &key (pre-visit #'visit-node)
				(post-visit #'identity))
  (df-explore-from-node (get-node graph node-label)
			:pre-visit pre-visit
			:post-visit post-visit))

(defun unvisit-nodes(graph)
  (dolist (node (graph-nodes graph))
    (setf (node-visited node) nil)))

(defun df-explore (graph &key (explore-fn #'df-explore-from-node)
			   (pre-visit #'visit-node)
			   (post-visit #'identity))
  (unvisit-nodes graph)
  (dolist (node (graph-nodes graph))
    (unless (node-visited node)
      (funcall explore-fn node :pre-visit pre-visit
	       :post-visit post-visit))))

;; DFS with explicit stack
(defun make-stack (&optional stack)
  #'(lambda (cmd &optional element)
      (ecase cmd
	(:test (endp stack))
	(:push (push element stack))
	(:pop (pop stack))
	(:get stack))))

(defun stack-empty-p (stack)
  (funcall stack :test))

(defun stack-push (stack element)
  (funcall stack :push element))

(defun stack-pop (stack)
  (funcall stack :pop))

(defun stack-get (stack)
  (funcall stack :get))

(defun get-unvisited-adj (node)
  (find-if-not #'node-visited (node-adj-nodes node)))

(defun edf-explore-from-node (node &key (pre-visit #'visit-node)
				     (post-visit #'identity))
  (let ((stack (make-stack)))
    (stack-push stack node)
    (funcall pre-visit node)
    (loop until (stack-empty-p stack)
       do (let* ((node (stack-pop stack))
		 (unvisited-adj (get-unvisited-adj node)))
	    (if unvisited-adj
		(progn (stack-push stack node)
		       (funcall pre-visit unvisited-adj)
		       (stack-push stack unvisited-adj))
		(funcall post-visit node))))))

;; adjacency
(defun is-adjacent-to (node1 node2)
  "can I go from node2 to node1?"
  (if (member node1 (node-adj-nodes node2))
      t
      nil))

(defun reciprocal-edges (node)
  (mapcar (lambda (adj-node)
	    (is-adjacent-to node adj-node))
	  (node-adj-nodes node)))

(defun vertices-reciprocal-p (node)
  "check if node is adjacent to all nodes adj to node."
  (notany #'null (reciprocal-edges node)))

(defun directedp (graph)
;;does a lot of duplicate work
  (some #'null (mapcar (lambda (node)
			   (vertices-reciprocal-p node))
			 (graph-nodes graph))))

;; connected components
(defun set-connected-component (node component-id)
  (setf (node-connected-component node) component-id))

(defun set-connected-component-and-visit (node component-id)
  (set-connected-component node component-id)
  (visit-node node))

(defun component-is (node ccid)
  (= (node-connected-component node) ccid))

(defun gather-component (graph-nodes ccid &optional (cc nil))
  "get all nodes which are part of connected component ccid."
  (if (endp graph-nodes)
      cc
      (gather-component (rest graph-nodes) ccid
			(cons-if (lambda (node)
				   (component-is node ccid))
				 (first graph-nodes) cc))))
    

(defun show-connected-components (graph
				  max-ccid
				  &optional (connected-components nil))
  (if (< max-ccid 0)
      connected-components
      (show-connected-components graph (1- max-ccid)
				 (cons (gather-component
					(graph-nodes graph) max-ccid)
				       connected-components))))

(defun connected-components (graph)
  "assign ccids to each node in graph."
  (let ((explore-calls 0))
    (unvisit-nodes graph) ;i don't seem able to reuse df-explore here
    (dolist (node (graph-nodes graph))
      (unless (node-visited node)
	(df-explore-from-node node :pre-visit
			      (lambda (node)
				(set-connected-component-and-visit
				 node explore-calls)))
	(incf explore-calls)))
  (show-connected-components graph (1- explore-calls))))

;; dags
(defun make-counter (count &optional end)
  #'(lambda (cmd)
      (ecase cmd
        (:test (and (numberp end) (> count end)))
        (:get count)
        (:next (prog1 count
                    (unless (and (numberp end) (> count end))
                      (incf count)))))))
;; adapted from https://web.archive.org/web/20110720015815/
;; https://www.cs.northwestern.edu/academics/courses/325/readings/graham/generators.html

(defun get-count (counter)
  (funcall counter :get))

(defun incf-count (counter)
  (funcall counter :next))

(defun empty-counter-p (counter)
  (funcall counter :test))

(defun previsit (node counter)
  (setf (node-pre node) (incf-count counter))
  (visit-node node))

(defun postvisit (node counter)
  (setf (node-post node) (incf-count counter)))

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
	(show-pre-post (rest graph-nodes)
		       (acons node (list (node-pre node)
					 (node-post node))
			      pre-post-list)))))

(defun pre-post-visit-nodes (graph &key (explore-fn
					 #'df-explore-from-node))
  "mark pre and post numbers in df exploration."
  (let ((counter (make-counter 0)))
    (reset-nodes-pre-post graph)
    (df-explore graph :explore-fn explore-fn
		:pre-visit (lambda (node) (previsit node counter))
		:post-visit (lambda (node) (postvisit node counter)))
  (show-pre-post (graph-nodes graph))))

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
      (dolist (adj (node-adj-nodes node))
	(when (back-edge? node adj)
	  (return-from any-back-edges? (list node adj)))))))

(defun is-dag? (graph)
  (pre-post-visit-nodes graph)
  (not (any-back-edges? graph)))

(defun linearize-visited-dag (graph)
  (sort (graph-nodes graph) #'> :key #'node-post))

(defun linearize-dag (graph)
  (pre-post-visit-nodes graph)
  (when (is-dag? graph)
    (linearize-visited-dag graph)))

;; tests
(defun all-visited? (graph)
  (notany #'null (mapcar (lambda (node)
			   (node-visited node))
			 (graph-nodes graph))))

(get-unvisited-adj ug (get-node ug 'e)) ; B
(is-adjacent-to (get-node ug 'c) (get-node ug 'a)) ; t
(is-adjacent-to (get-node ug 'a) (get-node ug 'c)) ; t
(is-adjacent-to (get-node dg 'c) (get-node dg 'd)) ; t
(is-adjacent-to (get-node dg 'd) (get-node dg 'c)) ; nil
(is-adjacent-to (get-node ug 'a) (get-node ug 'f)) ; nil
(vertices-reciprocal-p (get-node dg 'A)) ; nil
(vertices-reciprocal-p (get-node ug 'A)) ; t
(directedp ug) ; nil
(directedp dg) ; t
(connected-components ug) ; ((#N(E) #N(D) #N(C) #N(B) #N(A)) (#N(G) #N(F)))
(pre-post-visit-nodes ug)
(pre-post-visit-nodes ug :explore-fn #'edf-explore-from-node)
(any-back-edges? ug) ; (#n.B #n.D)
(is-dag? dag) ; t
(is-dag? ug) ; nil
