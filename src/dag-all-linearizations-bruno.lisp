;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

(load "graph-bruno.lisp")

;; is a given graph a dag?
(is-dag? ug) ; nil
(reset-nodes-pre-post ug) ; to reset graph
(is-dag? dag) ; t
(reset-nodes-pre-post dag)

;; (any) linearization

(linearize-dag dag)
(reset-nodes-pre-post dag)
(linearize-dag dag2)
(reset-nodes-pre-post dag2)

;; number of possible linearizations

(possible-linearizations-dag ug) ; error
(possible-linearizations-dag dag) ; 4
(possible-linearizations-dag dag) ; 72
