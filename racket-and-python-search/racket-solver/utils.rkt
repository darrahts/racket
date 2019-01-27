#lang racket
;; so we have more string functionality
(require srfi/13)
;; for queues
(require data/queue)
;; for priority queues
(require data/heap)

(provide (all-defined-out))

;; some functions for stacks, sets and queues
;; stacks
(define (make-stack)
  (list))

(define (push-stack x st)
      (cons x st))

(define (pop-stack st)
  (values (car st) (cdr st)))

(define (stack-empty? st)
  (empty? st))

;; queues
;; to make a new queue, use the make-queue function
(provide make-queue)

(define (push-queue x queue)
      (enqueue! queue x)
  queue)

(define (pop-queue queue)
  (let ([output (dequeue! queue)])
    (values output queue)))

(provide queue-empty?)

;; some more functions for viewing queues
(define (length-queue queue)
  (length (queue->list queue)))

(provide queue->list)

;; priority queues
(define (make-priority-queue priority-func)
  (make-heap (lambda (x y)
               (<= (priority-func x) (priority-func y)))))

(define (push-priority-queue x queue)
      (heap-add! queue x)
  queue)

(define (pop-priority-queue queue)
  (let ([output (heap-min queue)])
    (heap-remove-min! queue)
    (values output queue)))

(define (priority-queue-empty? queue)
  (equal? (heap-count queue) 0))

;; sets
(define (make-set)
	(list))

(define (ismember? elm lst)
  (ormap [lambda (val) (equal? val elm)] lst))

(define (push-set elm lst)
  (cons elm lst))

;; some constants
(define PACMAN #\P)
(define PACMAN-ALT #\<)
(define FOOD #\.)
(define WALL #\%)
(define SPACE #\space)

;; some functions to find things in the maze

(define (get-pos maze row col)
  (string-ref (list-ref maze row) col))

(define (find-pacman maze)
  (let ([output (index-of-not-false (map-maze maze (string PACMAN)))])
    (if (equal? output #f)
        (let ([output-alt (index-of-not-false (map-maze maze (string PACMAN-ALT)))])
          (if (equal? output-alt #f)
              (raise "Couldn't find Pacman")
              output-alt))
        output)))

(define (find-first-food maze)
  (index-of-not-false (map-maze maze (string FOOD))))

(define (map-maze maze elm)
  (map (lambda (row)
         (string-contains row elm)) maze))

(define (index-of-not-false lst)
  (let loop ((lst lst)
             (idx 0))
    (cond ((empty? lst) #f)
          ((not (equal? (first lst) #f)) (list idx (first lst)))
          (else (loop (rest lst) (add1 idx))))))

;; get successor states
(define (delete-pacman maze)
  (map (lambda (row)
         (if (string-contains? row (string PACMAN))
             (list->string (map (lambda (char) (if (equal? char PACMAN) SPACE char)) (string->list row)))
             (if (string-contains? row (string PACMAN-ALT))
                 (list->string (map (lambda (char) (if (equal? char PACMAN-ALT) SPACE char)) (string->list row)))
                 row)))
       maze))

(define (add-pacman maze  pac-row pac-col)
  (map (lambda (row-index)
         (if (equal? row-index pac-row)
             (list->string
              (map (lambda (col-index)
                     (if (equal? pac-col col-index)
                         PACMAN
                         (get-pos maze row-index col-index)))
                   (stream->list (in-range (string-length (list-ref maze row-index))))))
             (list-ref maze row-index)))
       (stream->list (in-range (length maze)))))

(define (get-succ maze)
  (let ([pac-pos (find-pacman maze)]
        [no-pacman-maze (delete-pacman maze)])
    (map (lambda (move)
           (let ([move-name (first move)]
                 [pac-row (+ (first pac-pos) (first (second move)))]
                 [pac-col (+ (second pac-pos) (second (second move)))]
                 )
             (list move-name (add-pacman no-pacman-maze pac-row pac-col))
             ))
         (filter (lambda (move)
                   (let ([move-name (first move)]
                         [pac-row (+ (first pac-pos) (first (second move)))]
                         [pac-col (+ (second pac-pos) (second (second move)))]
                         )
                     (not (equal? (get-pos maze pac-row pac-col) WALL))))
                 '(
                   (#\W (0 -1))
                   (#\E (0 1))
                   (#\N (-1 0))
                   (#\S (1 0))
                   ))
        )))

;; check if this is a goal state by checking if the amount of food is equal to zero
(define (is-goal maze)
  (equal?
   (length
    (filter
     (lambda (row)
       (string-contains row (string FOOD)))
     maze))
   0)
  )

;; some heuristics for A-star to use
(define (count-food maze)
  (apply + 
         (map (lambda (row)
                (length
                 (filter
                  (lambda (char) (equal? char FOOD))
                  (string->list row))))
              maze)))

(define (distance-to-food maze)
  (let ([pac-pos (find-pacman maze)]
        [food-pos (find-first-food maze)])
    (if (equal? food-pos #f)
        0
        (+ (abs (- (first pac-pos) (first food-pos))) (abs (- (second pac-pos) (second food-pos)))))))

(define (null-heuristic maze) 0)

;; a g-func to use with a priority queue for A*
;; this assumes each node is stored on the frontier as tuple of (<path to state>, state)
;; thus the g funciton for a node is length(path) + heuristic(state) or (+ (length (first node)) (heuritic-fun (second node)))
(define (g-func heurisitc-fun)
  (lambda (node)
    (+ (length (first node)) (heurisitc-fun (second node)))))

;; to read in the maze
(define (layout-path-parser path)
  (file->lines path #:mode 'text #:line-mode 'linefeed))

;; some functions just for printing things out nicely
(define (display-maze maze)
  (display
   (foldr
    (lambda (x y)
      (string-append x (string-append "\n" y)))
    "" maze)))

(define (display-succ succ)
  (display (first succ))
  (display "\n")
  (display-maze (second succ)))
