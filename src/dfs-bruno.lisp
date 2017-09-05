;; authors: bruno cuconato (@odanoburu)
;; placed in the public domain.

(load #p"graph-bruno.lisp")

;; show example graphs
(print ug)
(print dg)

;; recursive depth-first exploration
(df-explore ug :explore-fn #'df-explore-from-node)
(all-visited? ug)
(unvisit-nodes)

;; depth-first exploration with explicit stack
(df-explore ug :explore-fn #'edf-explore-from-node)
(all-visited? ug)
(unvisit-nodes)
