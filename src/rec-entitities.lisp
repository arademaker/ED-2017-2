;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

;; como lidar com pontuação? (mexer no input de entidades?) ou assim
;; tá ok?

(ql:quickload :cl-conllu)
(load #p"/home/bruno/git/ed-2017-2/src/trie.lisp")

(defun unwind-entity (entity)
  (when entity
    (if (trie-is-leaf? (first entity))
	(print (reverse entity))
	(unwind-entity (rest entity)))))

(defun get-token-form (token)
  (if (typep token 'cl-conllu:token)
      (cl-conllu:token-form token)
      (cl-conllu:mtoken-form token)))

(defun token-in-trie? (trie token)
  "checks if token is in trie, if it is, sees if it is followed by a
space and returns the space node, else returns the node. if it's not
in trie returns nil"
  (let ((trie-node (partially-in-trie? trie (get-token-form token))))
    (when trie-node
	(let* ((children (trie-children trie-node))
	       (space (find #\space children :key #'trie-value)))
	  (if space
	      space
	      (when (trie-is-leaf? trie-node)
		trie-node))))))

(defun aux-recognize-ents-in-tokens (trie token-list &optional entity)
  (let ((trie-path (token-in-trie? trie (first token-list))))
    (cond ((and (null entity)
		(null trie-path))
	   (values (rest token-list) nil))
	  ((and (null trie-path) entity)
	   (values token-list entity))
;; not (rest token-list) to give tk a fresh chance, but will only give
;; this chance to last tk. (think of searching for joão almeida de
;; castro when only joão almeida and joão almeida de silva are ents:
;; only castro will be reconsidered). is that a problem?
	  ((and trie-path entity)
	   (aux-recognize-ents-in-tokens (first trie-path)
					 (rest token-list)
					 (cons trie-path entity))))))

(defun recognize-ents-in-tokens (trie token-list)
  (when token-list
    (multiple-value-bind (rest-token-list entity)
	(aux-recognize-ents-in-tokens trie token-list)
      (unwind-entity entity)
      (recognize-ents-in-tokens trie rest-token-list))))

(defun construct-token-list-aux (tokens begin end
				 &key token-list mtoken)
  (when (endp tokens) (return-from construct-token-list-aux
			(values nil token-list)))
;; because an mtoken can be the last one in a sentence.
  (let* ((token (first tokens))
	(token-id (cl-conllu:token-id token))
	(rest-tokens (rest tokens)))
    (cond ((< token-id begin)
	   (construct-token-list-aux rest-tokens begin end
				     :token-list (cons token
						       token-list)
				     :mtoken mtoken))
	  ((= token-id begin)
	   (construct-token-list-aux rest-tokens begin end
				     :token-list (cons mtoken
						       token-list)))
	  ((and (> token-id begin) (<= token-id end))
	   (construct-token-list-aux rest-tokens begin end
				     :token-list token-list))
	  ((> token-id end)
	   (values tokens token-list)))))

(defun construct-token-list (tokens mtokens &optional token-list)
  "construct list of tokens, replacing tokens by their respective
mtokens: (pt em o governo) -> (pt no governo)"
  (if (endp mtokens)
      (append (reverse token-list) tokens)
      (let* ((mtoken (first mtokens))
	     (begin (cl-conllu:mtoken-start mtoken))
	     (end (cl-conllu:mtoken-end mtoken)))
	(multiple-value-bind (rest-tokens result-token-list)
	    (construct-token-list-aux tokens begin end :mtoken mtoken)
	  (construct-token-list rest-tokens (rest mtokens)
				(append result-token-list
					token-list))))))

(defun cons-tokens-from-sentence (sentence)
  (construct-token-list (cl-conllu:sentence-tokens sentence)
			(cl-conllu:sentence-mtokens sentence)))
    
(defun recognize-ents-in-sentence (trie sentence)
  (recognize-ents-in-tokens trie (cons-tokens-from-sentence sentence)))

(defun recognize-ents-in-sentences (trie sentences)
  (when sentences
    (recognize-ents-in-sentence trie (first sentences))
    (recognize-ents-in-sentences trie (rest sentences))))


;; tests
(let ((sents (cl-conllu:read-file #p"~/git/query-conllu/CF1.conllu"))
      (trie (start-trie (read-file "~/git/ed-2017-2/src/entities.txt"))))
  (recognize-ents-in-sentences trie sents))
