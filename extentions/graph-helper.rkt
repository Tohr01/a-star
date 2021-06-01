#lang racket

#|
    Carl Raabe
    30th May 2021
    Informatik eA | PP | Frau Berg

    Implements a small, simple graph libary using associationlists
    Node form: (node (x y ((neighbour1 edgeWeight1)...)))
|#

#|

Get specific data from nodes

|#

(define (get-neighbours node)
  (caddar(cdr node))
)

(define (get-x node)
  (caadr node)
)

(define (get-y node)
  (cadadr node)
)

(define (get-node node-identifier graph)
  (assoc node-identifier graph)
)

(provide 
  get-neighbours
  get-node
  get-x
  get-y
)