;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

;; encontrar submatches tb
;; transformar em função geradora


(ql:quickload :cl-conllu)
(compile-file #p"~/git/ed-2017-2/src/trie.lisp")
(load #p"~/git/ed-2017-2/src/trie.fasl")

;;
;; recognize entities
(defun unwind-entity (entity start size &optional entities)
  (unless entity
    (return-from unwind-entity entities))
  (let ((ent-id (trie-is-leaf? (first entity))))
    (if (numberp ent-id)
        (unwind-entity (rest entity) start (1- size)
                       (acons ent-id (list start size) entities))
        (unwind-entity (rest entity) start (1- size) entities))))

;;
;; recognize entities
(defun token-in-trie? (trie token)
  "return trie-leaf if token is leaf in trie."
  (partially-in-trie? trie token))

(defun aux-recognize-ents-in-sentence (trie token-list
                                       &key entity (iter 0))
  (when (endp token-list)
    (return-from aux-recognize-ents-in-sentence
      (values nil entity iter)))
  (let ((trie-node (token-in-trie? trie (first token-list))))
    (cond ((and (null entity)
		(null trie-node))
	   (values (rest token-list) nil (1+ iter)))
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

;;
;; entry point
(defun recognize-ents-in-sentences (trie sentences
                                    &key entities (sentid 0))
  "take trie and list of sentences (list of tokens), return sentid
with entities recognized in index ix (entity . ix)"
  (if (endp sentences)
      entities
      (let ((entities-in-sent (recognize-ents-in-sentence
                               trie (first sentences))))
        (recognize-ents-in-sentences trie (rest sentences)
                                     :entities (acons sentid
                                                      entities-in-sent
                                                      entities)
                                     :sentid (1+ sentid)))))

;;
;; visualization
(defun get-entities (entities entids &optional entities-found)
  "get entities' names from entity ids."
  (if (endp entids)
      entities-found
      (get-entities entities (rest entids)
                    (cons (nth (caar entids) entities)
                          entities-found))))

(defun visualize-entities-and-sentences (sentences rec-entities
                                         entities &optional viz)
  "useful for debugging. assumes sentences and rec-entities are
paired."
  (if (endp sentences)
      viz
      (let* ((entids-in-sent (cdar rec-entities))
             (ents-in-sent (get-entities entities entids-in-sent))
             (sent (first sentences)))
        (visualize-entities-and-sentences (rest sentences)
                                          (rest rec-entities)
                                          entities
                                          (acons ents-in-sent
                                                 sent
                                                 viz)))))

;;
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

;;
;; tests
#|
(join-words '("silva" "da" "lula")) ; "lula da silva"
(count-and-remove 1 (list 1 52 26 73 1 0) :predicate #'=) ; 2 (0 73 26 52)
(count-and-remove -88 (list 1 52 26 73 1 0) :predicate #'=) ; 0 (0 1...)
(count-and-remove-entity 1 (list 1 284 1 393 0 -1)
			 nil #'=) ; ((1 . 2)) (-1 0 393 284)
(count-and-remove-entities (list 1 -1)
			   (list 1 2 3 4 -1 1 8 -1 -1 3)
			   :predicate #'=) ;((-1 . 3) ((1 . 2))
(let* ((sents (cl-conllu:read-file #p"~/git/query-conllu/CF1.conllu"))
       (ents (read-entities #p"~/git/ed-2017-2/src/entities.txt"))
       (trie (start-trie ents)))
  (reset-entities)
  (recognize-ents-in-sentences trie sents)
  (count-and-remove-entities ents (show-entities)))
;; (("Lula" . 1) ("Fernando Henrique Cardoso" . 1) ("PT" . 4) ("secretaria municipal de zoologia" . 0))
|#
(let* ((raw-sents (cl-conllu:read-file #p"~/git/query-conllu/CF1.conllu"))
       (token-sents (cons-tokens-from-sentences raw-sents))
       (form-sents (forms-from-sentences token-sents))
       (char-sents (chars-from-sentences token-sents))
       (raw-ents (read-entities #p"~/git/ed-2017-2/src/entities.txt"))
       (ents (process-entities raw-ents))
       (trie (start-trie ents))
       (rec-entities (recognize-ents-in-sentences trie char-sents)))
  raw-sents
  token-sents
  form-sents
  char-sents
  raw-ents
  ents
  rec-entities
  (visualize-entities-and-sentences form-sents (reverse rec-entities) raw-ents)
  )
