#lang racket

(require "utils.rkt")


;(define maze (layout parser "file")) see doc 

;; A set solution for the tiny maze
(provide tinyMazeSearch)
(define (tinyMazeSearch maze)
  ;; a solution for the tiny maze layout
  '(#\S #\S #\W #\S #\W #\W #\S #\W))

;; solve using BFS
(provide BFS)

(define (BFS maze)
  ; make the fronteir
  (define fronteir (make-queue))
  ; each node is a list of lists (the path to it and itself)
  (define node (list (list) null))
  ; the start node has no path to it and itself is the maze
  (define start (list (list) maze))
  ; add the start node to the fronteir
  (set! fronteir (push-queue start fronteir))
  ; list for visited nodes
  (define visited (list))

  ; separate the node into the maze portion (self)
  ; and the path portion (dir)
  (define node-self (list-ref start 1))
  (define node-dir (list-ref start 0))

  (println node-self)
  (println node-dir)
  (for ([i (in-naturals 0)]
        ;break if the goal is found or the queue is empty
        #:break (or (is-goal node-self) (queue-empty? fronteir)))
    (set!-values (node fronteir) (pop-queue fronteir))
    ;(set! node (pop-queue fronteir))
    (println node)
    ; get the maze part of the node (the node itself)
    (set! node-self (list-ref node 1))
    (println node-self)
    ; get the path part of the node
    (set! node-dir (list-ref node 0))
    (println node-dir)

    (cond
      [(is-goal node-self) (println "found goal!")]
      [else
       (map
        (lambda (i) 
          (cond
            ; check if the node is on the visited list
             [(not (ismember? (list-ref i 1) visited))
              ; if its not, get its successors and add them to the queue
              (get-succ node-self)
              (set! fronteir (push-queue (list-ref i 1) fronteir))
              ; add the node to the set of visited nodes
              (set! visited (push-queue node-self visited))]
             [else (set! fronteir fronteir) ])
          )
        )
      ]))

  )
    
    



   
  
 ; '(#\S #\S #\W #\S #\W #\W #\S #\W))

;; solve using DFS
(provide DFS)

(define (DFS maze)
  ;; your code here
  '(#\S #\S #\W #\S #\W #\W #\S #\W))

;; solve using A*
(provide A-star)

(define (A-star maze heuristic-fun)
  ;; your code here
  '(#\S #\S #\W #\S #\W #\W #\S #\W))


(define maze (layout-path-parser "../layouts/tinyMaze.lay"))

(BFS maze)



