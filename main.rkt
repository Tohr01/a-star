#lang racket

#|
    Carl Raabe
    30th May 2021
    Informatik eA | PP | Frau Berg

    A implementation of the A* Algorithm
|#

;; TODO init costs
;; Parse closeslist

; –– Requirements ––
(require "./extentions/graph-helper.rkt")
(require "./extentions/priority-queue.rkt")

; –– Graph ––
(define g '(
            ;("a" (0 15 (("b" 10) ("c" 20))))
            ;("b" (10 3 (("a" 10) ("d" 10))))
            ;("c" (14 1 (("a" 20) ("d" 10))))
            ;("d" (11 5 (("c" 10) ("d" 10))))
            ("a" (0 0 (("b" 10) ("c" 10))))
            ("b" (10 3 (("a" 10) ("c" 10))))
            ("c" (3 10 (("a" 10) ("d" 10) ("f" 20))))
            ("d" (10 10 (("c" 10) ("f" 10))))
            ("e" (15 5 (("b" 30) ("f" 10))))
            ("f" (13 15 (("c" 20) ("d" 10) ("e" 10))))
            )
  )


(define
    (a-star start-node target-node openlist closedlist graph)
 1   
)

#|
    predecessor-node: String e.g.: "a"
    currentNode: String e.g.: "a"
    target-node: String e.g: "a"
    openlist: associationlist as described in priority-queue.rkt
    closedlist: associationlist as described in priority-queue.rkt
    graph: associationlist as described in graph-helper.rkt
|# 
(define (explore-node currentNode target-node openlist closedlist graph)
    (cond
        [(empty? openlist) "error"]
        [(equal? (caar openlist) target-node) closedlist]
        (else
            (explore-node
                (caar openlist)
                target-node
                (remove (assoc currentNode openlist) (explore-neighbours currentNode (get-neighbours (get-node currentNode graph)) target-node openlist graph))
                ;; fix
                (cons (assoc currentNode openlist) closedlist)
                graph
            )
        )
    )
)

#|
    predecessor-node: "a"
    currentNode: "a"
    currentNodeNeighbours: '(("b" 10) ("c" 20))
    target-node: "a"
    openlist

    1. nachbar ist in liste und kennzahl ist kleiner als in openlist
    2. nachbar ist in list und kennzahl ist größer als in openlist
        - kosten um zu knoten zu kommen sind niedriger -> kosten ersetzen
        - kosten nicht niedriger -> nichts
    3. nachbar ist nicht in list -> fügen liste hinzu mit nachbar-id ...
|#
(define (explore-neighbours currentNode currentNodeNeighbours target-node openlist graph)
    (cond
    [(empty? currentNodeNeighbours) openlist]
    [(isMember? (caar currentNodeNeighbours) openlist) 
        ;; Case 1
        (if (< (calc-f (get-node (caar currentNodeNeighbours) graph) (+ (get-cost (caar currentNodeNeighbours) openlist) (cadar currentNodeNeighbours)) (get-node target-node graph)) (get-heuristic (assoc (caar currentNodeNeighbours) openlist)))
            (explore-neighbours 
                currentNode 
                (cdr currentNodeNeighbours) 
                target-node 
                (add (construct-elem 
                         (caar currentNodeNeighbours) 
                         (calc-f (get-node (caar currentNodeNeighbours) graph) (+ (get-cost (caar currentNodeNeighbours) openlist) (cadar currentNodeNeighbours)) (get-node target-node graph))
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
                target-node 
                (if (< (+ (get-cost (caar currentNodeNeighbours) openlist) (cadar currentNodeNeighbours)) (get-cost (caar currentNodeNeighbours) openlist)) (set-cost (caar currentNodeNeighbours) (+ (get-cost (caar currentNodeNeighbours) openlist) (cadar currentNodeNeighbours)) openlist) openlist)
                graph
            )
        )]
        ;; Case 3
        (else
            (explore-neighbours 
                currentNode 
                (cdr currentNodeNeighbours) 
                target-node
                (add (construct-elem (caar currentNodeNeighbours) (calc-f (get-node (caar currentNodeNeighbours) graph) (+ (get-cost currentNode openlist) (cadar currentNodeNeighbours)) (get-node target-node graph)) (+ (get-cost currentNode openlist) (cadar currentNodeNeighbours)) currentNode) openlist)     
                graph)
        )
    )
)

#|
    ––––––––––––––––
    Helper functions
    ––––––––––––––––
|#


(define (calc-f node costs targetNode)
    (+ costs (beeline (get-x node) (get-x targetNode) (get-y node) (get-y targetNode)))
)

#|
Calculate the beeline of a given node to the target node
|#
(define (beeline x1 x2 y1 y2)
    (sqrt (+ (expt (abs (- x1 x2)) 2) (expt (abs (- y1 y2)) 2)))
)

;–––––––––––––––
;–––– DEBUG ––––
;–––––––––––––––

(explore-node "a" "f" (add (construct-elem "a" 15.620499351813308 0 "a") '()) '() g)
;(explore-neighbours "a" (get-neighbours (get-node "a" g)) "d" '(("a" (15.620499351813308 0 "a"))) g)

;(calc-f (get-node (caar '(("b" 10) ("c" 20))) g) (+ 0 (cadar '(("b" 10) ("c" 20)))) (get-node "d" g))