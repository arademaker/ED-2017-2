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


;; input
(defun str-to-char (string)
  (coerce string 'list))

(defun remove-separator (chars separator)
  (remove-if (lambda(character)
               (char= character separator))
            chars))

(defun process-string (line separator)
  (remove-separator (str-to-char (string-trim '(#\space #\tab) line))
                    separator))

(defun read-entities (filepath &optional (separator #\space))
  "return (ent-id . entity), where entity = list of characters minus
separator"
  (with-open-file (stream filepath)
    (loop for line = (read-line stream nil)
       for ix from 0
       while line
       collect (cons ix (process-string line separator)))))


;; search
(defun aux-search-children (tries character)
  (when (endp tries)
    (return-from aux-search-children nil))
  (let ((trie (first tries)))
    (if (char= character (trie-value trie))
	trie
	(aux-search-children (rest tries) character))))

(defun search-children (trie character)
  "searches children of a trie node for a value of character"
  (let ((trie-children (trie-children trie)))
    (aux-search-children trie-children character)))

(defun search-trie (trie chars &optional path (ix 0))
  (when (endp chars)
    (return-from search-trie (values trie path ix)))
  (let ((match (search-children trie (first chars))))
    (if (null match)
	(values trie path ix) ; returns reverse path
	(search-trie match (rest chars)
                     (cons match path) (1+ ix)))))

(defun str-search-trie (trie string &optional (separator #\space))
  (let ((chars (process-string string separator)))
    (search-trie trie chars)))

(defun in-trie? (trie chars)
  "return nil or trie-leaf."
  (multiple-value-bind (trie-node * ix) (search-trie trie chars)
    (when (and (= (length chars) ix)
               (trie-is-leaf? trie-node))
      trie-node)))

(defun str-in-trie? (trie string &optional (separator #\space))
  (let ((chars (process-string string separator)))
    (in-trie? trie chars)))


;; cons trie
(defun add-char-to-children (trie char)
  (push (make-trie :value char) (trie-children trie))
  (search-children trie char))

(defun mark-node (trie ent-id)
  (setf (trie-is-leaf? trie) ent-id)
  trie)

(defun insert-node (trie chars ent-id)
  "add value which is not present in trie."
  (if (endp chars)
      (mark-node trie ent-id)
      (insert-node (add-char-to-children trie (first chars))
		   (rest chars) ent-id)))

(defun aux-add-node (trie chars ent-id)
  (multiple-value-bind (trie-node * ix)
      (search-trie-aux trie chars)
    (if (= (length chars) ix)
        (mark-node trie-node ent-id)
        (insert-node trie-node (subseq chars ix) ent-id))))

(defun add-node (trie value)
  (destructuring-bind (ent-id . entity-chars) value
    (aux-add-node trie entity-chars ent-id)))
    
(defun construct-trie (root-trie values)
  (if (endp values)
      root-trie
      (progn (add-node root-trie (first values))
	     (construct-trie root-trie (rest values)))))

(defun start-trie (ent-values)
  (let ((trie-root (make-trie)))
    (construct-trie trie-root ent-values)))

;; tests

(let* ((list-ents (read-entities #p"/home/bruno/git/ed-2017-2/src/entities.txt"))
       (test-trie (start-trie list-ents)))
  (trie-is-leaf? test-trie) ; nil
  (search-trie test-trie "amanda") ; |a| (|a| |d| |n| |a| |m| |a|) 6
  (search-trie test-trie "xesus") ; |ROOT| NIL 0
  (search-trie test-trie "amanda silva") ; |a| (|a| |v| |l| ... |a|) 12
  (partially-in-trie?path test-trie "amanda") ; (|a| |d| ... |m| |a|)
  (value-in-trie? test-trie "amanda") ; nil
  (partially-in-trie?path test-trie "amanda silva") ; (|a| |v| |l| ...)
  (str-in-trie? test-trie "amanda silva") ; |a| leaf
  (str-in-trie? test-trie "amanda silvan") ; nil
  )
