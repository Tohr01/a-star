#lang racket

#|
    Carl Raabe
    30th May 2021
    Informatik eA | PP | Frau Berg

    A implementation of the A* Algorithm in Scheme
|#

; –– Requirements ––
(require "./extentions/graph-helper.rkt")
(require "./extentions/priority-queue.rkt")
(require "./extentions/closedlist-parser.rkt")

; –– Graph ––
(define g '(
            ("a" (0 0 (("b" 10) ("c" 10))))
            ("b" (10 3 (("a" 10) ("e" 30))))
            ("c" (3 10 (("a" 10) ("d" 10) ("f" 20))))
            ("d" (10 10 (("c" 10) ("f" 10))))
            ("e" (15 5 (("b" 30)("f" 10))))
            ("f" (13 15 (("c" 20) ("d" 10) ("e" 10))))
            )
  )

#|
    Main A*-Algorithm

    startNode: node-identifier (see priority-queue.rkt) (start)
    targetNode: node-identifier (see priority-queue.rkt) (target)
    graph: graph (see graph-helper.rkt)

    Returns: String (optimal path)
|#
(define (a-star startNode targetNode graph)
    (get-path-from (explore-node 
        targetNode 
        (add 
            (construct-elem 
                startNode 
                (calc-f (get-node startNode graph) 0 (get-node targetNode graph))
                0 
                startNode) 
            '()) 
        '() g) startNode)
)

#|
    Explores nodes in openlist. Returns closedlist when targetNode

    targetNode: String e.g: "a"
    openlist: associationlist as described in priority-queue.rkt
    closedlist: associationlist as described in priority-queue.rkt
    graph: associationlist as described in graph-helper.rkt
|# 
(define (explore-node targetNode openlist closedlist graph)
    (cond
        [(empty? openlist) '()]
        [(equal? (caar openlist) targetNode) (cons (car openlist) closedlist)]
        (else
            (explore-node
                targetNode
                (explore-neighbours (caar openlist) (get-neighbours (get-node (caar openlist) graph)) targetNode openlist graph)
                (cons (assoc (caar openlist) openlist) closedlist)
                graph
            )
        )
    )
)

#|
    Explores Neighbours for given node, adds them to the openlist or replaces the cost value when lower
    
    Cases:  
        1. neighbour is in list and key number is smaller than in openlist
        2. neighbour is in list and key number is larger than in openlist
            - costs to get to nodes are lower -> replace costs
            - costs higher -> nothing
        3. neighbour is not in list -> add to list with neighbour-id ...

    Params:
        currentNode: node-identifier (see priority-queue.rkt) (currentNode which is currently being investigated)
        currentNodeNeighbours node-identifier (see graph-helper.rkt) (neighbours of currentNode)
        targetNode: node-identifier (see-priority-queue.rkt)
        openlist: openlist (see priority-queue.rkt)
        graph: graph (see graph-helper.rkt)

    Returns: updated openlist
|#
(define (explore-neighbours currentNode currentNodeNeighbours targetNode openlist graph)
    (cond
    [(empty? currentNodeNeighbours) (remove (assoc currentNode openlist) openlist)]
    [(isMember? (caar currentNodeNeighbours) openlist) 
        ;; Case 1
        (if (< (calc-f (get-node (caar currentNodeNeighbours) graph) (+ (get-cost (caar currentNodeNeighbours) openlist) (cadar currentNodeNeighbours)) (get-node targetNode graph)) (get-heuristic (assoc (caar currentNodeNeighbours) openlist)))
            (explore-neighbours 
                currentNode 
                (cdr currentNodeNeighbours) 
                targetNode 
                (add (construct-elem 
                         (caar currentNodeNeighbours) 
                         (calc-f (get-node (caar currentNodeNeighbours) graph) (+ (get-cost (caar currentNodeNeighbours) openlist) (cadar currentNodeNeighbours)) (get-node targetNode graph))
                             (if (< (+ (get-cost (caar currentNodeNeighbours) openlist) (cadar currentNodeNeighbours)) (get-cost (caar currentNodeNeighbours) openlist))
                                 (+ (get-cost (caar currentNodeNeighbours) openlist) (cadar currentNodeNeighbours))
                                 (get-cost (caar currentNodeNeighbours) openlist)
                             )
                         currentNode
                ) openlist)
                graph)
            ;; Case 2
            (explore-neighbours 
                currentNode 
                (cdr currentNodeNeighbours) 
                targetNode 
                (if (< (+ (get-cost (caar currentNodeNeighbours) openlist) (cadar currentNodeNeighbours)) (get-cost (caar currentNodeNeighbours) openlist)) (set-cost (caar currentNodeNeighbours) (+ (get-cost (caar currentNodeNeighbours) openlist) (cadar currentNodeNeighbours)) openlist) openlist)
                graph
            )
        )]
        ;; Case 3
        (else
            (explore-neighbours 
                currentNode 
                (cdr currentNodeNeighbours) 
                targetNode
                (add (construct-elem (caar currentNodeNeighbours) (calc-f (get-node (caar currentNodeNeighbours) graph) (+ (get-cost currentNode openlist) (cadar currentNodeNeighbours)) (get-node targetNode graph)) (+ (get-cost currentNode openlist) (cadar currentNodeNeighbours)) currentNode) openlist)     
                graph)
        )
    )
)

#|
    ––––––––––––––––
    Helper functions
    ––––––––––––––––
|#


#|
    Calculates the value of f(n) by adding g(n) and h(n) together

    node: currentNode to be processed

    Returns: Number (fValue) 
|#
(define (calc-f node costs targetNode)
    (+ costs (beeline (get-x node) (get-x targetNode) (get-y node) (get-y targetNode)))
)

#|
    Calculate the hypotenuse of a right-angled triangle

    Returns: Number (Length of hypotenuse)
|#
(define (beeline x1 x2 y1 y2)
    (sqrt (+ (expt (abs (- x1 x2)) 2) (expt (abs (- y1 y2)) 2)))
)