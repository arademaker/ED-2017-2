;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

;; resolver pontuação.
;; mudar trie para colocar em is-leaf? o id da entitidade.
;; transformar em função geradora

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

;; process entities: join names, add them to entities-found, do things
;; to them

(defun unwind-entity (entity start end &optional entities)
  (unless entity
    (return-from unwind-entity entities))
  (let ((ent-id (trie-is-leaf? (first entity))))
    (if ent-id
        (unwind-entity (rest entity) start (1- end)
                       (acons ent-id (list start end) entities))
        (unwind-entity (rest entity) start (1- end)))))


;; recognize entities

(defun token-in-trie? (trie token)
  "checks if token is in trie, if it is, sees if it is followed by a
space and returns the space node, else returns the node. if it's not
in trie returns nil"
  (let ((trie-node (partially-in-trie?node trie token)))
    (when trie-node
	(let* ((children (trie-children trie-node))
	       (space (find #\space children :key #'trie-value)))
	  (if space
	      (values space token)
	      (when (trie-is-leaf? trie-node)
		(values trie-node token)))))))

(defun aux-recognize-ents-in-sentence (trie token-list
                                       &key entity (iter 0))
  (when (endp token-list)
    (return-from aux-recognize-ents-in-sentence
      (values nil entity iter)))
  (let ((trie-node (token-in-trie? trie (first token-list))))
    (cond ((and (null entity)
		(null trie-node))
	   (values (rest token-list) nil iter))
	  ((and (null trie-node) entity)
	   (values token-list entity iter))
;; not (rest token-list) to give tk a fresh chance, but will only give
;; this chance to the last tk. (think of searching for joão almeida de
;; castro when only joão almeida and joão almeida de silva are ents:
;; only castro will be reconsidered, not 'de'). is that a problem?
	  (trie-node
	   (aux-recognize-ents-in-sentence trie-node
					 (rest token-list)
					 :entity (cons trie-node
                                                       entity)
                                         :iter (1+ iter))))))

(defun recognize-ents-in-sentence (trie token-list
                                   &key entities (start 0))
  "return entities found (list (cons ent-id (start end)))"
  (if (endp token-list)
      entities
      (multiple-value-bind (rest-token-list entity iter)
          (aux-recognize-ents-in-sentence trie token-list)
        (recognize-ents-in-sentence trie
                                    rest-token-list
                                    :entities
                                    (append (unwind-entity
                                             entity start iter)
                                            entities)
                                    :start (+ start iter)))))


;; entry point

(defun recognize-ents-in-sentences (trie sentences
                                    &key entities (sentid 0))
  "take trie and list of sentences (list of tokens), return sentid
with entities recognized in index ix (entity . ix)"
  (if (endp sentences)
      entities
      (let ((entities-in-sent (recognize-ents-in-sentence
                               trie (first sentences))))
        (recognize-ents-in-sentences trie
                                     (rest sentences)
                                     :entities (acons
                                                sentid
                                                entitites-in-sent
                                                entities)
                                     :sentid (1+ sentid)))))

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
(join-words '("silva" "da" "lula")) ; "lula da silva"
(count-and-remove 1 (list 1 52 26 73 1 0) :predicate #'=) ; 2 (0 73 26 52)
(count-and-remove -88 (list 1 52 26 73 1 0) :predicate #'=) ; 0 (0 1...)
(count-and-remove-entity 1 (list 1 284 1 393 0 -1)
			 nil #'=) ; ((1 . 2)) (-1 0 393 284)
(count-and-remove-entities (list 1 -1)
			   (list 1 2 3 4 -1 1 8 -1 -1 3)
			   :predicate #'=) ;((-1 . 3) ((1 . 2))
(let* ((sents (cl-conllu:read-file #p"~/git/query-conllu/CF1.conllu"))
       (ents (read-file "~/git/ed-2017-2/src/entities.txt"))
       (trie (start-trie ents)))
  (reset-entities)
  (recognize-ents-in-sentences trie sents)
  (count-and-remove-entities ents (show-entities)))
;; (("Lula" . 1) ("Fernando Henrique Cardoso" . 1) ("PT" . 4) ("secretaria municipal de zoologia" . 0))
