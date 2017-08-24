;; prepare input: get tokens and mtokens from sentence (specific to
;; cl-conllu)

(ql:quickload :cl-conllu)
(compile-file #p"~/git/ed-2017-2/src/trie.lisp")
(load #p"~/git/ed-2017-2/src/trie.fasl")

(defun get-token-form (token)
  (etypecase token
    (cl-conllu:token (cl-conllu:token-form token))
    (cl-conllu:mtoken (cl-conllu:mtoken-form token))))

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

(defun process-form (token)
  (process-string (get-token-form token)))

(defun cons-tokens-from-sentence (sentence)
  "this pre-processes cl-conllu classes for input"
  (mapcar #'process-form (construct-token-list
                            (cl-conllu:sentence-tokens sentence)
                            (cl-conllu:sentence-mtokens sentence))))

(let ((sents (cl-conllu:read-file #p"~/git/query-conllu/CF1.conllu")))
  (mapcar #'cons-tokens-from-sentence sents))
