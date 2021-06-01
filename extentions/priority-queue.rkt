#lang racket

#|
    Carl Raabe
    30th May 2021
    Informatik eA | PP | Frau Berg

    Form: '((key (heuristic cost predecessor-node))
    Implements a simple priority queue for an associationlist
|#
;;EXAMPLE REMOVE!! '(("a" (120 10 "b")) ("b" (120 10 "a")) ("d" (90 10 "b")))
; temporary list used by some functions
(define templist '())

#|
  Returns a queue as a list with elements sortet by heuristics

  elem: list of form '((node (heuristic cost predecessor-node)) -> z.B.: '(("a" (120 10 "b"))
  queue: queue of form '((node (heuristic cost predecessor-node) ... '((key (heuristic cost predecessor-node)))

  returns: updated queue
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

(define (get-cost node-identifier queue) 
  (cadadr (assoc node-identifier queue))
)

(define (set-cost node-identifier cost queue)
  (insert-at-index
    (remove (assoc node-identifier queue) queue)
    (list node-identifier (list (get-heuristic (assoc node-identifier queue)) cost (get-predecessor (assoc node-identifier queue))))
    (get-index-of node-identifier queue 0)
  )
)

(define (isMember? node-identifier queue)
  (if (assoc node-identifier queue) #t #f)
)

#|
––––––––––––––––––––––––
    List management
––––––––––––––––––––––––
|#

(define
  (insert-at-index list value index)
  (if (= index 0)
      (cons value list)
      (cons (car list) (insert-at-index (cdr list) value (- index 1)))
      )
  )

(define
  (get-index-of-<-order list value index)
  (cond
    [(empty? list) index]
       [(<= value (car list)) index]
       [(>= value (car list)) (get-index-of-<-order (cdr list) value (+ index 1))]
     )
    )

(define (get-index-of node-identifier queue index)
(if (equal? (assoc node-identifier queue) (car queue))
  index
  (get-index-of node-identifier (cdr queue) (+ index 1))
)
)

(define
(conv-assoc-list-to-heuristic associationlist)
  (if (empty? associationlist) templist
    (append (cons (caadar associationlist) templist) (conv-assoc-list-to-heuristic (cdr associationlist))) 
))


(define (get-elem-string elem)
  (car elem)
)

(define (get-predecessor elem)
  (car(cddadr elem))
)

(define (construct-elem node-identifier heuristic cost predecessor-node) 
    (list node-identifier (list heuristic cost predecessor-node))
)

#|
  Returns heuristic for given elem

  elem: Has to has format mentionend above. E.g.: ("a" (250 10 "b"))

  Returns: Number
|#
(define
(get-heuristic elem)
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