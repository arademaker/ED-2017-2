;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

(defstruct (trie (:print-function
		   (lambda (trie stream k)
		     (identity k)
		     (format stream "t#(~A)" (trie-root trie)))))
  (root nil))

(defstruct (node (:print-function
		   (lambda (node stream k)
		     (identity k)
		     (format stream "n\"~A\"" (node-value node)))))
  (value (error "must specify value for node.")))

(defun read-file (filepath)
  (with-open-file (stream filepath)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun search-trie (trie value)
  (rest (assoc value trie :test #'char=)))

(defun insert-node-aux (trie node-value &optional result)
  (if (= (length node-value) 0)
      (acons (first result) (rest result) trie)
      (insert-node-aux trie (subseq node-value 1)
		       (cons (char node-value 0) result))))

(defun insert-node (trie node-value)
  (insert-node-aux trie (reverse node-value)))

(defun add-node (trie node-value)
  (let* ((curr (char node-value 0))
	(searched-trie (search-trie trie curr)))
    (if (null searched-trie)
	(insert-node trie node-value)
	(acons curr (cons searched-trie
			  (add-node searched-trie
				    (subseq node-value 1)))
	       trie))))
  

(defun construct-trie (trie node-values)
  (if (endp node-values)
      trie
      (add-node trie node-value)))

(defun start-trie (node-values)
  (let ((trie (make-trie :root nil)))
    (construct-trie trie node-values)))
    

(defun search-similarities (entity entities &optional nodes)
  (let* ((other-entity (first entities))
	 (intersec (string<= entity other-entity)))
    (if (= intersec 0)
	nodes
	(search-similarities entity (rest entities)
			     (cons (list (subseq entity 0 intersec)
					 entity other-entity)
				   nodes)))))

(defun construct-trie (entities &optional (trie nil) (predicate #'string<))
  (let ((entity (first entities)))
    (search-similarities entity entities)

(defun join-string (list &optional (delimiter #\ ))
  (with-output-to-string (stream)
    (join-to-stream stream list delimiter)))

(defun join-to-stream (stream list &optional (delimiter #\&))
  (destructuring-bind (&optional first &rest rest) list
    (when first
      (write-string first stream)
      (when rest
        (write-char delimiter stream)
        (join-to-stream stream rest delimiter)))))
