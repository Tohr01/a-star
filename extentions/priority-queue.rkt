#lang racket

#|
    Carl Raabe
    30th May 2021
    Informatik eA | PP | Frau Berg

    Form: '((node-identifier (heuristic cost predecessor-node))
    Implements a simple priority queue for an associationlist with some additional management functions
|#

; –– temporary list used by some functions ––
(define templist '())

#|
  Returns a queue as a list with elements sortet by heuristics

  elem: list of form '((node (heuristic cost predecessor-node)) -> z.B.: '(("a" (120 10 "b"))
  queue: queue of form '((node (heuristic cost predecessor-node) ... '((key (heuristic cost predecessor-node)))

  returns: queue
|#
(define (add elem queue)
  (if (assoc (get-elem-string elem) queue)
    (if (< (get-heuristic elem) (get-heuristic(assoc (get-elem-string elem) queue)))
      (insert-at-index 
        (remove (assoc (get-elem-string elem) queue) queue) 
        elem 
        (get-index-of-<-order (conv-assoc-list-to-heuristic (remove (assoc (get-elem-string elem) queue) queue)) (get-heuristic elem) 0)
      )
      queue
    )
   (insert-at-index 
        queue
        elem 
        (get-index-of-<-order (conv-assoc-list-to-heuristic queue) (get-heuristic elem) 0)
      )
  )
)

#|
  Gets cost for given in node-identifier in queue

  node-identifier: String
  node-identifier: queue

  Returns: Number (Cost of node)
|#
(define (get-cost node-identifier queue) 
  (cadadr (assoc node-identifier queue))
)

#|
  Sets cost for given node-identifier in queue

  node-identifier: String
  cost: number
  queue: queue

  Returns: new queue with new cost for given node-identifier
|#
(define (set-cost node-identifier cost queue)
  (insert-at-index
    (remove (assoc node-identifier queue) queue)
    (list node-identifier (list (get-heuristic (assoc node-identifier queue)) cost (get-predecessor (assoc node-identifier queue))))
    (get-index-of node-identifier queue 0)
  )
)

#|
  Checks if node is part of queue

  node-identifier: String
  queue: queue

  Returns: boolean
|#
(define (isMember? node-identifier queue)
  (if (assoc node-identifier queue) #t #f)
)

#|
––––––––––––––––––––––––
    List management
––––––––––––––––––––––––
|#

#|
  Insert value at index in list

  list: list
  value: any
  index: number (where element should be inserted)

  Returns: list with inserted element
|#
(define
  (insert-at-index list value index)
  (if (= index 0)
      (cons value list)
      (cons (car list) (insert-at-index (cdr list) value (- index 1)))
      )
  )

#|
  Get index of value, where list[...] < value < list[...]

  list: list of numbers
  value: number
  index: number (default 0) 

  Return index
|#
(define
  (get-index-of-<-order list value index)
  (cond
    [(empty? list) index]
       [(<= value (car list)) index]
       [(>= value (car list)) (get-index-of-<-order (cdr list) value (+ index 1))]
     )
    )


#|
  Get index of node in queue

  node-identifier: String
  queue: queue
  index: number (default 0)

  Returns index of node in queue
|#
(define (get-index-of node-identifier queue index)
  (if (equal? (assoc node-identifier queue) (car queue))
    index
    (get-index-of node-identifier (cdr queue) (+ index 1))
  )
)

#|
  Converts queue to list of f values -> '(x y z ...)

  queue: queue

  Returns: list of numbers
|#
(define (conv-assoc-list-to-heuristic queue)
  (if (empty? queue) templist
    (append (cons (caadar queue) templist) (conv-assoc-list-to-heuristic (cdr queue))) 
))

#|
  elem: node

  Returns: node-identifier of node
|#
(define (get-elem-string elem)
  (car elem)
)

#|
  elem: node

  Returns: node-identifier of node
|#
(define (get-predecessor elem)
  (car(cddadr elem))
)

#|
  Constructs element of form '(node-identifier (fValue cost predecessor-node))

  node-identifier: String
  heuristic: number (value of f(n))
  cost: number
  predecessor-node: String

  Returns elem
|#
(define (construct-elem node-identifier heuristic cost predecessor-node) 
    (list node-identifier (list heuristic cost predecessor-node))
)

#|
  Returns f for given elem

  elem: Has to has format mentionend above. E.g.: ("a" (250 10 "b"))

  Returns: Number
|#
(define (get-heuristic elem)
  (caadr elem)
)

; Providing functions to other files
(provide 
  add
  isMember?
  construct-elem
  get-cost
  set-cost
  get-heuristic
  get-predecessor
)