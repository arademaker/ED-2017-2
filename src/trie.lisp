;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

;; any node at any point of a trie is itself a trie (as a trie is
;; characterized by a root node and its children.

(defstruct (trie (:print-function
		   (lambda (node stream k)
		     (identity k)
		     (format stream "|~A|" (trie-value node)))))
  (value "ROOT")
  (children)
  (is-leaf?))

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

;; maybe divide in two?
(defun search-trie-aux (trie chars &optional path (ix 0))
  (when (endp chars)
    (return-from search-trie-aux (values trie path ix)))
  (let ((match (search-children trie (first chars))))
    (if (null match)
	(values trie path ix) ; returns reverse path
	(search-trie-aux match (rest chars)
			 (cons match path) (1+ ix)))))

(defun search-trie (trie value)
  (search-trie-aux trie (str-to-char value)))

(defun partially-in-trie?path (trie value)
  (multiple-value-bind (* path ix) (search-trie trie value)
    (when (= (length value) ix) ; checking for length is enough
      path)))

(defun partially-in-trie?node (trie value)
  (multiple-value-bind (trie-node * ix) (search-trie trie value)
    (when (= (length value) ix) ; checking for length is enough
      trie-node)))

(defun value-in-trie? (trie value)
  "returns nil or leaf"
  (multiple-value-bind (trie-node * ix) (search-trie trie value)
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

(defun add-node (trie value)
  (let ((value (str-to-char value)))
    (multiple-value-bind (trie-node * ix) (search-trie-aux trie value)
      (if (= (length value) ix)
	  trie-node
	  (insert-node trie-node (subseq value ix))))))
    

(defun start-trie (node-values)
  (let ((trie-root (make-trie)))
    (dolist (v node-values trie-root)
      (add-node trie-root v))))
