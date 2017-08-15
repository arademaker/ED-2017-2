;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

;; any node at any point of a trie is itself a trie (as a trie is
;; characterized by a root node and its children.
(defstruct (trie (:print-function
		   (lambda (node stream k)
		     (identity k)
		     (format stream "n\"~A\"" (trie-value node)))))
  (value 'ROOT)
  (children)
  (is-leaf?))

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

(defun search-trie-aux (trie chars &optional (ix 0))
  (when (endp chars)
    (return-from search-trie-aux (values trie ix)))
  (let ((match (search-children trie (first chars))))
    (if (null match)
	(values trie ix)
	(search-trie-aux match (rest chars) (1+ ix)))))
;; returns number of chars to see if perfect match or not

(defun search-trie (trie value)
  (search-trie-aux trie (str-to-char value)))

(defun partially-in-trie? (trie value)
  (multiple-value-bind (trie-node ix) (search-trie trie value)
    (when (= (length value) ix)
      trie-node)))

(defun value-in-trie? (trie value)
  (multiple-value-bind (trie-node ix) (search-trie trie value)
    (when (and (= (length value) ix)
	       (trie-is-leaf? trie-node))
      trie-node)))

(defun add-char-to-children (trie char)
  (push (make-trie :value char) (trie-children trie))
  (search-children trie char))

(defun insert-node (trie value)
  "add value which is not present in trie."
  (if (endp value)
      (progn (setf (trie-is-leaf? trie) t)
	     trie)
      (insert-node (add-char-to-children trie (first value))
		   (rest value))))

(defun add-node-aux (trie value)
  (multiple-value-bind (result-trie ix)
	(search-trie-aux trie value)
    (if (= (length value) ix)
	trie
	(insert-node result-trie (subseq value ix)))))

(defun add-node (trie value)
  (let ((value (str-to-char value)))
    (add-node-aux trie value)
    trie)) ;; returns root instead of leaf
    
(defun construct-trie (trie values)
  (if (endp values)
      trie
      (construct-trie (add-node trie (first values))
		      (rest values))))

(defun start-trie (node-values)
  (let ((trie (make-trie)))
    (construct-trie trie node-values)))

;; tests

(let* ((list-trie (list "amanda silva" "amanda silvana"
				 "amanda silvana da silva"
				 "armando silva"
				 "júpiter"
				 "secretaria municipal de cultura"
				 "secretaria municipal de zoologia"))
       (test-trie (start-trie list-trie)))
  (is-leaf? test-trie) ; nil
  (search-trie test-trie "amanda") ; n"a" 6
  (search-trie test-trie "amanda silva") ; n"a" 12
  (partially-in-trie? test-trie "amanda") ; t
  (value-in-trie? test-trie "amanda") ; nil
  (partially-in-trie? test-trie "amanda silva") ; t
  (value-in-trie? test-trie "amanda silva") ; t
  (value-in-trie? test-trie "amanda silvan") ; nil
  )
