#lang racket
;; so we can make things pretty when we print them
(require racket/pretty)
;; to interpret mazes and stuff
(require "utils.rkt")
;; this is your code
(require "student.rkt")

(define layout-path-parameter (make-parameter "../layouts/tinyMaze.lay"))
(define search-strategy-parameter (make-parameter "dfs"))
(define search-heuristic-parameter (make-parameter ""))

(define parser
  (command-line
   #:once-each
   [("-l" "--layout") LAYOUT-PATH-PARAMETER
                      "maze layout filepath"
                      (layout-path-parameter LAYOUT-PATH-PARAMETER)]
   [("-s" "--strategy") SEARCH-STRATEGY-PARAMETER
                      "strategy for solving maze"
                      (search-strategy-parameter SEARCH-STRATEGY-PARAMETER)]
   [("-r" "--heuristic") SEARCH-HEURISTIC-PARAMATER
                      "hueristic for A*"
                      (search-heuristic-parameter SEARCH-HEURISTIC-PARAMATER)]))



;; parse a maze from a path and solve it
(define heuristic
  (if (equal? (search-heuristic-parameter) "count-food")
      count-food
      distance-to-food))

(define search-method
  (cond
    ((equal? (search-strategy-parameter) "bfs") BFS)
    ((equal? (search-strategy-parameter) "astar") (lambda (maze) (A-star maze heuristic)))
    ((equal? (search-strategy-parameter) "tinyMazeSearch") (lambda (maze) (tinyMazeSearch maze)))
    (else DFS)))

(display (list->string (search-method (layout-path-parser (layout-path-parameter)))))


