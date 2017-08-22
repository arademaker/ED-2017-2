;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

;; como lidar com pontuação? (mexer no input de entidades?) ou assim
;; tá ok?

(ql:quickload :cl-conllu)
(compile-file #p"~/git/ed-2017-2/src/trie.lisp")
(load #p"~/git/ed-2017-2/src/trie.fasl")

#|;;not used
(defun make-entities(&optional entities)
  #'(lambda (cmd &optional entity)
      (ecase cmd
        (:push (push entity entities))
        (:get entities))))

(defun entities-push (entities entity)
  (funcall entities :push entity))

(defun entities-get (entities)
  (funcall entities :get))
|#

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
      (subseq joined-words 1) ;to remove first space
      (join-words (rest words) (concatenate 'string (list #\space)
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
space and returns the space node, else returns the node. if it's not
in trie returns nil"
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
					 (acons trie-node token
						entity))))))

(defun recognize-ents-in-tokens (trie token-list)
  (when token-list
    (multiple-value-bind (rest-token-list entity)
	(aux-recognize-ents-in-tokens trie token-list)
      (unwind-entity entity)
      (recognize-ents-in-tokens trie rest-token-list))))

;; prepare input: get tokens and mtokens from sentence

(defun aux-construct-token-list (tokens start end
				 &key token-list mtoken)
  (when (endp tokens) (return-from aux-construct-token-list
			(values nil token-list)))
;; because an mtoken can be the last one in a sentence.
  (let* ((token (first tokens))
	(token-id (cl-conllu:token-id token))
	(rest-tokens (rest tokens)))
    (cond ((< token-id start)
	   (aux-construct-token-list rest-tokens start end
				     :token-list (cons token
						       token-list)
				     :mtoken mtoken))
	  ((= token-id start)
	   (aux-construct-token-list rest-tokens start end
				     :token-list (cons mtoken
						       token-list)))
	  ((and (> token-id start) (<= token-id end))
	   (aux-construct-token-list rest-tokens start end
				     :token-list token-list))
	  ((> token-id end)
	   (values tokens token-list)))))

(defun construct-token-list (tokens mtokens &optional token-list)
  "construct list of tokens, replacing tokens by their respective
mtokens: (pt em o governo) -> (pt no governo)"
  (if (endp mtokens)
      (append (reverse token-list) tokens)
      (let* ((mtoken (first mtokens))
	     (start (cl-conllu:mtoken-start mtoken))
	     (end (cl-conllu:mtoken-end mtoken)))
	(multiple-value-bind (rest-tokens result-token-list)
	    (aux-construct-token-list tokens start end :mtoken mtoken)
	  (construct-token-list rest-tokens (rest mtokens)
				(append result-token-list
					token-list))))))

(defun cons-tokens-from-sentence (sentence)
  (construct-token-list (cl-conllu:sentence-tokens sentence)
			(cl-conllu:sentence-mtokens sentence)))

;; entry points

(defun recognize-ents-in-sentence (trie sentence)
  (recognize-ents-in-tokens trie
                            (cons-tokens-from-sentence sentence)))

(defun recognize-ents-in-sentences (trie sentences)
  (when sentences
    (recognize-ents-in-sentence trie (first sentences))
    (recognize-ents-in-sentences trie (rest sentences))))

;; count entities (remove is for better performance)

(defun count-and-remove (entity entities-found
			 &key (predicate #'string=)
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

(defun count-and-remove-entity (entity entities-found
				entity-count predicate)
  (multiple-value-bind (count filtered-entities)
      (count-and-remove entity entities-found :predicate predicate)
    (values (acons entity count entity-count) filtered-entities)))

(defun count-and-remove-entities (entities entities-found
				  &key (predicate #'string=)
				    entity-count)
  (if (endp entities)
      entity-count
      (multiple-value-bind (new-entity-count filtered-entities)
	  (count-and-remove-entity (first entities) entities-found
				   entity-count predicate)
	(count-and-remove-entities (rest entities) filtered-entities
				   :predicate predicate
				   :entity-count new-entity-count))))

;; tests

;; (join-words '("silva" "da" "lula")) ; "lula da silva"
;; (count-and-remove 1 (list 1 52 26 73 1 0) :predicate #'=) ; 2 (0 73 26 52)
;; (count-and-remove -88 (list 1 52 26 73 1 0) :predicate #'=) ; 0 (0 1...)
;; (count-and-remove-entity 1 (list 1 284 1 393 0 -1)
;; 			 nil #'=) ; ((1 . 2)) (-1 0 393 284)
;; (count-and-remove-entities (list 1 -1)
;; 			   (list 1 2 3 4 -1 1 8 -1 -1 3)
;; 			   :predicate #'=) ;((-1 . 3) ((1 . 2))
;; (let* ((sents (cl-conllu:read-file #p"~/git/query-conllu/CF1.conllu"))
;;        (ents (read-file "~/git/ed-2017-2/src/entities.txt"))
;;        (trie (start-trie ents)))
;;   (reset-entities)
;;   (recognize-ents-in-sentences trie sents)
;;   (count-and-remove-entities ents (show-entities)))

