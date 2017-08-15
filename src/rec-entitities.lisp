;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

;; como lidar com pontuação? (mexer no input de entidades?)

(ql:quickload :cl-conllu)
(load #p"/home/bruno/git/ed-2017-2/src/trie.lisp")

(defun unwind-entity (entity)
  (when entity
    (if (trie-is-leaf? (first entity))
	(reverse entity)
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
  (if token-list
      (let ((trie-node (token-in-trie? trie (first token-list))))
	(cond ((and (null entity) (null trie-node))
	       (aux-recognize-ents-in-tokens trie (rest token-list)))
	      ((and (null trie-node) entity)
	       (values token-list (unwind-entity entity)))
	      ((and trie-node)
	       (aux-recognize-ents-in-tokens trie-node (rest token-list)
					     (cons trie-node entity)))))
      (values nil (unwind-entity entity))))

(defun recognize-ents-in-tokens (trie token-list)
  (when token-list
    (multiple-value-bind (rest-token-list entity)
	(aux-recognize-ents-in-tokens trie token-list)
      (print entity)
      (recognize-ents-in-tokens trie rest-token-list))))

(defun construct-token-list-aux (tokens begin end
				 &key token-list mtoken)
  ;; because an mtoken can be the last one in a sentence
  (when (endp tokens) (return-from construct-token-list-aux
			(values nil token-list)))
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
