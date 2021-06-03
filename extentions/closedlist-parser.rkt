#lang racket

#|
  Carl Raabe
  3rd June 2021
  Informatik eA | PP | Frau Berg

  Parses openlist and creates readable String representing the path A* calculated
|#

(require "./priority-queue.rkt")

#|
  Returns readable String by processing the closedlist

  closedlist: closedlist (for format see priority-queue.rkt)
  startNode: node-identifier (see priority-queue.rkt)

  returns: String (path and costs as text)
|#
(define (get-path-from closedlist startNode)
    (if (empty? closedlist) 
      "no path found"
      (conv-to-string (parse-nodes (caar closedlist) startNode closedlist '()) (get-cost (caar closedlist) closedlist) "Der Weg ist -> ")
    )
)

#|
  Creates readable String by combining pathlist and cost together

  pathlist: path from startNode to targetNode
  cost: Number (total cost)
  text: String

  Returns: text 
|#
(define (conv-to-string pathlist cost text)
  (if (empty? pathlist) 
    (string-append text (string-append ". Mit den insgesamten Kosten von " (number->string cost)))
    (conv-to-string (cdr pathlist) cost (string-append text (string-append (car pathlist) " ")))
  )
)

#|
  Returns direct path from startNode to targetNode

  currentNode: node-identifier (see priority-queue.rkt)
  startNode: node-identifier (see priority-queue.rkt) (startNode)
  closedlist: closedlist (for format see priority-queue.rkt)
  templist: list

  Returns: list of numbers
|#
(define (parse-nodes currentNode startNode closedlist templist)
  (if (equal? currentNode startNode) 
    (cons startNode templist)
    (parse-nodes (get-predecessor (assoc currentNode closedlist)) startNode closedlist (cons currentNode templist))
  )
)

; Providing function to other files
(provide get-path-from)
