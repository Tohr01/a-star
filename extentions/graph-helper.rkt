#lang racket

#|
    Carl Raabe
    30th May 2021
    Informatik eA | PP | Frau Berg

    Implements a small, simple graph libary using associationlists
    Node form: (node-identfier (x y ((neighbour1 edgeWeight1)...)))
    -> Neighbours: '((neighbour1 edgeWeight1)...))
|#

#|

Get specific data from nodes

|#

#|
  node: Node

  Returns: list of neighbours for given node
|#
(define (get-neighbours node)
  (caddar(cdr node))
)

#|
  node: Node

  Returns: x pos of given node
|#
(define (get-x node)
  (caadr node)
)

#|
  node: Node

  Returns: y pos of given node
|#
(define (get-y node)
  (cadadr node)
)

#|
  Returns node for given identifier

  node-identifier: String
  graph: graph (described in graph-helper.rkt)

  Returns: Node
|#
(define (get-node node-identifier graph)
  (assoc node-identifier graph)
)

; Providing functions to other files
(provide 
  get-neighbours
  get-node
  get-x
  get-y
)