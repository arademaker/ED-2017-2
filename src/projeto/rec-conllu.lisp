;; prepare input: get tokens and mtokens from sentence (specific to
;; cl-conllu)

;; this code adapts rec-entities.lisp for use with the :cl-conllu
;; library. it will build the list of sentences getting the forms from
;; their tokens, substituting tokens by mtokens when
;; necessary. sentence lists can be lists of lists of chars for input
;; to rec-entities.lisp, or lists of strings for human consumption.

;; use (dir-recognize-entities dir-path entities-path) to recognize
;; entities in all *.conllu files in a given directory, using the
;; entity list at entities-path. the output will be as

;; ((file-id (sent-id (ent-id index))))

;; (dir-entities-not-found ) and (dir-count-entities ) will return
;; their namesakes. the dir-prefix in these functions indicate that
;; files are being read from a directory one at a time, so that the
;; memory heap does not exhaust. they are also adapted to remove the
;; unnecessary (in their use cases) file-id, which the original
;; functions in rec-entities.lisp don't handle.

;; in the end of the file there are examples of entity recognition and
;; count in conllu files.

;; problem: when working with mtokens mixed with tokens, the indices
;; returned by recognize-ents-in-sentences might not be the real
;; indices.


(ql:quickload :cl-conllu)
(ql:quickload :alexandria)

(load (compile-file #P"~/git/ed-2017-2/src/projeto/rec-entities.lisp"))

;;
;; from sentences to list of strings
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
  (construct-token-list (cl-conllu:sentence-tokens sentence)
                        (cl-conllu:sentence-mtokens sentence)))

(defun cons-tokens-from-sentences (sentences)
  (mapcar #'cons-tokens-from-sentence sentences))

(defun forms-from-sentence (sentence)
  (mapcar #'get-token-form sentence))

(defun forms-from-sentences (sentences)
  (mapcar #'forms-from-sentence sentences))

(defun chars-from-sentence (sentence)
  (mapcar #'process-form sentence))

(defun chars-from-sentences (sentences)
  (mapcar #'chars-from-sentence sentences))


;;
;; reading
(defun chars-in-file (filepath)
  "read conllu file and return lists of chars for each sentence."
  (let* ((raw-sents (cl-conllu:read-file filepath))
        (token-sents (cons-tokens-from-sentences raw-sents))
        (char-sents (chars-from-sentences token-sents)))
    char-sents))

(defun trie-from-entities (path)
  (let* ((raw-ents (read-entities path))
        (ents (process-entities raw-ents))
        (trie (start-trie ents)))
    (values trie ents)))

(defun aux-dir-recognize-entities (trie file-paths &optional entities)
  (if (endp file-paths)
      entities
      (let* ((file-path (first file-paths))
             (file-id (file-namestring file-path))
             (chars-sents (chars-in-file file-path))
             (sent-entities (recognize-ents-in-sentences trie
                                                         chars-sents)))
        (aux-dir-recognize-entities trie (rest file-paths)
                                    (acons file-id sent-entities
                                           entities)))))

(defun dir-recognize-entities (dir-path entities-path)
  "recognize entities in all .conllu files in a directory. (this reads
one file at time, which prevents stack overflow."
  (multiple-value-bind (trie *) (trie-from-entities entities-path)
    (let ((file-paths (directory dir-path)))
      (aux-dir-recognize-entities trie file-paths))))

;;
;; entity statistics
(defun get-entids-from-entrecs-with-fileid (entrecs)
  (alexandria:mappend (lambda (entrec) (get-entids-from-entrecs
                                        (rest entrec)))
                      entrecs))

(defun dir-entities-not-found (dir-path entities-path)
  (let ((entrecs (dir-recognize-entities dir-path entities-path)))
    (ents-not-found (get-entids-from-entrecs-with-fileid entrecs)
                    (get-number-of-entities entities-path))))
;; (mapcar (lambda (entid) (get-entity raw-ents entid)) *) can make
;; list using entities' names and not id's

(defun dir-count-entities (dir-path entities-path)
  (count-entities
   (get-entids-from-entrecs-with-fileid
    (dir-recognize-entities dir-path entities-path))))
      
;;
;; tests
#|
(let* ((raw-sents (cl-conllu:read-file
                   #p"/home/bruno/docs/dhbb-sample/2.conllu"))
       (token-sents (cons-tokens-from-sentences raw-sents))
       (form-sents (forms-from-sentences token-sents))
       (char-sents (chars-from-sentences token-sents))
       (raw-ents (read-entities #p"entities.txt"))
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
  (viz-count raw-ents (count-entities
                       (get-entids-from-entrecs rec-entities))))

(let ((raw-ents (read-entities #p"path to entities list")))
           (with-open-file (stream "~/resultado.txt"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (format stream (write-to-string (viz-count raw-ents (count-entities
                                         (get-entids-from-entrecs (alexandria:mappend (lambda (entrec) (rest entrec)) (dir-recognize-entities #p"path to conllu files"  #p"path to entities list")))))))))
|#
