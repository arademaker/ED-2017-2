;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

;; any node at any point of a trie is itself a trie (as a trie is
;; characterized by a root node and its children
(defstruct (trie (:print-function
		   (lambda (node stream k)
		     (identity k)
		     (format stream "n\"~A\"" (trie-value node)))))
  (value)
  (children))

(defun read-file (filepath)
  (with-open-file (stream filepath)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun str-to-char (string)
  (coerce string 'list))

(defun search-children-aux (tries character)
  (when (endp tries)
    (return-from search-children-aux nil))
  (let ((trie (first tries)))
    (if (char= character (trie-value trie))
	trie
	(search-children-aux (rest tries) character))))

(defun search-children (trie character)
  "searches children of a trie node for a value of character"
  (let ((trie-children (trie-children trie)))
    (search-children-aux trie-children character)))

(defun search-trie-aux (trie value &optional (chars 0))
  (let ((result (search-children trie (first value))))
    (if (or (endp value) (null result))
	(values result chars)
	(search-trie-aux result (rest value) (1+ chars)))))

(defun search-trie (trie value)
  (search-trie-aux trie (str-to-char value)))

(defun add-char-to-children (trie char)
  (push (make-trie :value char) (trie-children trie)))

(defun insert-node (trie value)
  (if (endp value)
      trie
      (insert-node (add-char-to-children trie (first value))
		   (rest value))))

(defun add-node (trie value)
  (let ((value (str-to-char value)))
    (multiple-value-bind (result-trie ix)
	(search-trie-aux trie value)
    (if (= (length value) ix)
	trie
	(insert-node result-trie (subseq value ix))))))

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
