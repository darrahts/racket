#lang racket
;; so we can randomly choose a move
(require racket/random)

(require "typed-othello-game-logic.rkt")
(require "utils.rkt")

(provide (all-defined-out))

(define (random-strategy player board)
  "A strategy that randomly chooses a move."
  (first (legal-moves player board)))

(define (maximize-difference player board)
  "A strategy that maximizes the difference in pieces."
  ((maximizer count-difference) player board))

(define (maximize-weight player board)
  "A strategy that maximizes the weighted difference in pieces."
  ((maximizer weighted-squares) player board))


;;This is a recursive algorithm that evaluates the board when depth reaches 0
;;and then generates the values of the nodes as it comes out of recursion
;;if there are no legal moves it returns the value of the board
;;otherwise it finds the best move and returns it
(define (minimax player board depth eval-fn)
    [if (= depth 0) ;if the depth is 0 then 
        [values (eval-fn player board) #f] ;evauluate the board
        [if (any-legal-move? player board) ;otherwise does the player have any moves? if so then
            [let ((moves (legal-moves player board)) ;get the players moves, assign some locals
                  (best-move #f)
                  (best-val -2000001))    
                     (for ((move moves)) ;loop through the moves, assign the current value and move to a minimax recursion
                          (let-values ([(cur-val cur-move) (minimax (opponent player) (make-move move player board) (- depth 1) eval-fn)])
                          (when (> (- cur-val) best-val) ;when the current val is better than the best val
                               (set! best-val (- cur-val)) ;assign it to best val
                               (set! best-move move)))) ;do the same for best move
                 (values best-val best-move)] ;return the best-val and best-move
             [if (any-legal-move? (opponent player) board) ;if the player doesnt have any moves, does the opponent?
                 (values (- 0 (eval-fn (opponent player) board)) #f) ;return the opponents best score if so
                 (values (eval-fn player board) #f)]]]) ;otherwise the game is over!


(define (minimax-searcher depth eval-fn)
  "A strategy that searches DEPTH levels and then uses EVAL-FN."
  (let ([output (lambda (player board)
                  (let-values ([(value move) (minimax player board depth eval-fn)])
                    move))])
    (set! output (procedure-rename output
                                   (string->symbol
                                    (string-append
                                     "minimax-searcher-"
                                     (number->string depth)
                                     "-"
                                     (symbol->string
                                      (object-name eval-fn))))))
    output))

; same as minimax but will cutoff branches of the search tree to limit the
; search space
(define (alpha-beta player board achievable cutoff depth eval-fn)
    [if (= depth 0) ;if the depth is 0 then 
        [values (eval-fn player board) #f] ;evauluate the board
        [if (any-legal-move? player board) ;otherwise does the player have any moves? if so then
            [let ((moves (legal-moves player board)) ;get the players moves, assign some locals
                  (best-move #f)
                  (best-val -2000001))    
                     (for ((move moves)) ;loop through the moves and break when achievable is greater than the cutoff
                       #:break (>= achievable cutoff) ;otherwise flip the negated cutoff and achievable and run alphabeta on the opponent 
                       (let-values ([(cur-val cur-move) (alpha-beta (opponent player) (make-move move player board) (- cutoff) (- achievable) (- depth 1) eval-fn)])
                           (set! achievable (- cur-val)) ;update achievable since it wasn't bigger than the cutoff
                           (when (> (- cur-val) best-val) ;when the current val is better than the best val
                               (set! best-val (- cur-val)) ;assign it to best val
                               (set! best-move move)))) ;do the same for best move
                 (values best-val best-move)] ;return the best-val and best-move after searching through the tree (i.e. the for loop)
             [if (any-legal-move? (opponent player) board) ;however, if the player doesnt have any moves, does the opponent?
                 (values (- 0 (eval-fn (opponent player) board)) #f) ;return the opponents best score if so
                 (values (final-value player board) #f)]]]) ;otherwise the game is over!




(define (alpha-beta-searcher depth eval-fn)
  "A strategy that searches to DEPTH and then uses EVAL-FN."
  (let ([output (lambda (player board)
                  (let-values ([(value move) (alpha-beta player board losing-value winning-value depth eval-fn)])
                    move))])
    (set! output (procedure-rename output
                                   (string->symbol
                                    (string-append
                                     "alpha-beta-searcher-"
                                     (number->string depth)
                                     "-"
                                     (symbol->string
                                      (object-name eval-fn))))))
    output))


;((minimax-searcher 4 weighted-squares) WHITE (layout-path-parser "layouts/test-hard-#f-68.lay"))
;((minimax-searcher 4 weighted-squares) BLACK (layout-path-parser "layouts/test-hard-#f-68.lay"))
;((alpha-beta-searcher 4 weighted-squares) WHITE (layout-path-parser "layouts/test-hard-#f-68.lay"))
;((alpha-beta-searcher 4 weighted-squares) BLACK (layout-path-parser "layouts/test-hard-#f-68.lay"))


