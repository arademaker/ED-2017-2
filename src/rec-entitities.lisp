;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

;; como lidar com pontuação? (mexer no input de entidades?) ou assim
;; tá ok?

(ql:quickload :cl-conllu)
(load (compile-file #p"trie.lisp"))

(let (entities)
  (defun insert-entity (entity)
    (push entity entities))
  (defun show-entities ()
    entities)
  (defun reset-entities ()
    (setf entities nil)))


;; process entities: join names, add them to entities-found, do things
;; to them

(defun join-words (words &optional joined-words)
  ;; equiv to (format nil "~{~A~^ ~}" words), but faster
  (if (endp words)
      (subseq joined-words 1)		
      (join-words (rest words)
		  (concatenate 'string (list #\space)
			       (first words)
			       joined-words))))

(defun get-token-form (token)
  (etypecase token
    (cl-conllu:token (cl-conllu:token-form token))
    (cl-conllu:mtoken (cl-conllu:mtoken-form token))))

(defun process-entity (entity)
  (insert-entity (join-words (mapcar (lambda (node-token)
                                       (get-token-form
                                        (rest node-token)))
                                     entity))))

(defun unwind-entity (entity)
  (when entity
    (if (trie-is-leaf? (caar entity))
	(process-entity entity))
	(unwind-entity (rest entity))))


;; recognize entities

(defun token-in-trie? (trie token)
  "checks if token is in trie, if it is, sees if it is followed by a
   space and returns the space node, else returns the node. if it's
   not in trie returns nil"
  (let ((trie-node (partially-in-trie?node trie
					    (get-token-form token))))
    (when trie-node
	(let* ((children (trie-children trie-node))
	       (space (find #\space children :key #'trie-value)))
	  (if space
	      (values space token)
	      (when (trie-is-leaf? trie-node)
		(values trie-node token)))))))


(defun aux-recognize-ents-in-tokens (trie token-list &optional entity)
  (when (endp token-list)
    (return-from aux-recognize-ents-in-tokens (values nil entity)))
  (multiple-value-bind (trie-node token)
      (token-in-trie? trie (first token-list))
    (cond ((and (null entity)
		(null trie-node))
	   (values (rest token-list) entity))
	  ((and (null trie-node) entity)
	   (values token-list entity))
	  ;; not (rest token-list) to give tk a fresh chance, but will only give
	  ;; this chance to the last tk. (think of searching for joão almeida de
	  ;; castro when only joão almeida and joão almeida de silva are ents:
	  ;; only castro will be reconsidered, not 'de'). is that a problem?
	  (trie-node
	   (aux-recognize-ents-in-tokens trie-node
					 (rest token-list)
					 (acons trie-node token entity))))))

(defun recognize-ents-in-tokens (trie token-list)
  (when token-list
    (multiple-value-bind (rest-token-list entity)
	(aux-recognize-ents-in-tokens trie token-list)
      (unwind-entity entity)
      (recognize-ents-in-tokens trie rest-token-list))))


;; entry points

(defun recognize-ents-in-sentence (trie sentence)
  (recognize-ents-in-tokens trie (cl-conllu:sentence-tokens sentence)))

(defun recognize-ents-in-sentences (trie sentences)
  (dolist (sent sentences)
    (recognize-ents-in-sentence trie sent)))


;; count entities (remove is for better performance)

(defun count-and-remove (entity entities-found &key (predicate #'string=)
						 (count 0) filtered-entities)
  (when (endp entities-found) (return-from count-and-remove
				(values count filtered-entities)))
  (let ((entity-found (first entities-found))
	(rest-entities-found (rest entities-found)))
    (if (funcall predicate entity entity-found)
	(count-and-remove entity rest-entities-found
			  :predicate predicate
			  :count (1+ count)
			  :filtered-entities filtered-entities)
	(count-and-remove entity rest-entities-found
			  :predicate predicate
			  :count count
			  :filtered-entities
                          (cons entity-found
                                filtered-entities)))))


(defun count-and-remove-entity (entity entities-found entity-count predicate)
  (multiple-value-bind (count filtered-entities)
      (count-and-remove entity entities-found :predicate predicate)
    (values (acons entity count entity-count) filtered-entities)))


(defun count-and-remove-entities (entities entities-found &key (predicate #'string=) entity-count)
  (if (endp entities)
      entity-count
      (multiple-value-bind (new-entity-count filtered-entities)
	  (count-and-remove-entity (first entities) entities-found
				   entity-count predicate)
	(count-and-remove-entities (rest entities) filtered-entities
				   :predicate predicate
				   :entity-count new-entity-count))))

;; tests

(defun ents ()
  (with-open-file (in #P"~/work/cpdoc/dhbb/dic/pessoa-individuo.txt")
    (loop for line = (read-line in nil nil)
	  while line
	  collect (format nil "~{~A~^ ~}"
			  (expand-names (cl-ppcre:split "[ ]+" (string-trim '(#\Space #\Tab) line)))))))

(defun expand-names (names &optional res)
  (if (null names)
      (nreverse res)
      (expand-names (cdr names)
		    (cond ((member (car names) '("dos" "do")
				   :test #'equal)
			   (cons "o" (cons "de" res)))
			  ((member (car names) '("das" "da")
				   :test #'equal)
			   (cons "a" (cons "de" res)))
			  (t (cons (car names) res))))))

(defun teste ()
  (let* ((files (append (directory #P"/Users/arademaker/work/cpdoc/dhbb-nlp/udp/?.conllu")
			(directory #P"/Users/arademaker/work/cpdoc/dhbb-nlp/udp/1?.conllu")))
	 (sents (reduce (lambda (l a) (append l (cl-conllu:read-conllu a))) files :initial-value nil))
	 (ents (ents))
	 (trie (start-trie ents)))
    (reset-entities)
    (recognize-ents-in-sentences trie sents)
    (count-and-remove-entities ents (show-entities))))
