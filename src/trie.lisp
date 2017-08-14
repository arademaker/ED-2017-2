;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

(defstruct (trie (:print-function
		   (lambda (trie stream k)
		     (identity k)
		     (format stream "t#(~A)" (trie-root trie)))))
  (root nil)
  (predicate #'string<))

(defstruct (node (:print-function
		   (lambda (node stream k)
		     (identity k)
		     (format stream "n\"~A\"" (node-value node)))))
  (value (error "must specify value for node."))
  (children nil))

(defun split-by-one-space (string)
    "Returns a list of substrings of string divided by ONE space each.
Note: Two consecutive spaces will be seen as if there were an empty
string between them."
    ;; from cl-cookbook
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
       while j))

(defun read-file (filepath)
  (with-open-file (stream filepath)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun read-entities (filepath)
  (mapcar #'split-by-one-space (read-file filepath)))

(defun search-similarities (entity entities &optional nodes)
  (let* ((other-entity (caar entities))
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
