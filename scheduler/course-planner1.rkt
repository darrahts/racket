#lang racket

(require "read-in.rkt")
(require "schedule-tester.rkt")
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


(define (schedule-advisor catalog goal-conditions network interactive)
  ;; Your code here
  (values '() 0))

(define (get-expected-values schedule network grades-for-taken-classes)
  ;; Your code here
  '())

;;
(define (get-expected-gpa schedule network grades-for-taken-classes)
  ;; Your code here
  0)

(define catalog (preq-list-parser "preq-lists/all-cs-courses.txt"))
(define network (bayesian-network-parser "bayesian-networks/all-cs-courses.txt"))
;(define network (bayesian-network-parser "bayesian-networks/simple.txt"))

(define goalconditions (list 'CS4959 'CS1151))

;(define goalconditions (list 'CSmathematics 'CS4959 'CS1151))
;(define goalconditions (list 'CSmajor))
(define (DFS goals catalog)

  ;;define the goal stack
  (define goalStack (make-stack))
  (set! goalStack goalconditions)
  ;;create a list for visitedNodes
  (define visitedNodes (make-stack))
  ;;create a list of all the courses I need
  (define neededClasses (make-stack))
  ;define a var for current, options, currentCourse, preReqs, courseType
  (define current null)
  (define options null)
  (define currentCourse null)
  (define prereqs null)
  (define courseType null)
  ;loop
  (for ([i (in-naturals 0)]
    #:break (stack-empty? goalStack)) ;while the goal stack is not empty

    ;;get the current at the top of the stack
    (set!-values (current goalStack) (pop-stack goalStack))
    ;;add it to the list of visitedNodes
    (set! visitedNodes (push-stack current visitedNodes))
    ;;get the options associated with that node
    (set! options (hash-ref catalog current))
    ;for now i'm just going to consider the first option need to revise this in the future
    (set! currentCourse (list-ref options 0))
    ;get the prerequisites for the course
    (set! prereqs (course-preq currentCourse))
    ;get the type of the course
    (set! courseType (course-type currentCourse))
    ;;if its a course add it to the classes we need to take list if its not there
    (cond [(and (not (ismember? currentCourse neededClasses)) (is-class? currentCourse)) (set! neededClasses (push-stack currentCourse neededClasses))])    
    (fprintf (current-output-port) "(course Name: ~a | Course Prereqs: ~a | Course Type: ~a).\n" (course-name currentCourse) prereqs courseType)
    ;add all the prerequsites that have not been visited to the stack
    (map (lambda (i)

           (cond [(ismember? i visitedNodes) (set! goalStack goalStack)] [else  (set! goalStack (push-stack i goalStack))])


           ) prereqs)
    
    )
  ;visitedNodes
  neededClasses
  )

(define (is-class? course)
  (equal? (course-type course) 'COURSE))

(define (ismember? elm lst)
    (ormap [lambda (val) (equal? val elm)] lst))

(define (is-scheduled? class sched)
    (define on-schedule #f)
    (for ((sem sched)
          #:break (eq? #t on-schedule))
          (cond
              [(ismember? class sem) (set! on-schedule #t)]
              [else (set! on-schedule on-schedule)]
          )
     )
     on-schedule
)


;(define cs1151 (course-name (list-ref courses 0)))
;(println "a")
;(println cs1151)
;(set! belief (get-beliefs cs1151 network (course-preq (list-ref (hash-ref catalog cs1151) 0))))
;(println belief)
;(println "b")

(define belief null)
(define condition null)
(define courses (DFS goalconditions catalog))
(define cs-net null)
(define grades null)

;get the expected grade of a class given a list of beliefs 
(define (get-expected-grade list-of-beliefs)
  (define max 0)
  (define grade 0)
  (define prob 0)
  (for ((b list-of-beliefs))
    (set! prob (belief-prob b))
    (cond
      [(> prob max) (set! max prob) (set! grade (belief-grade b))]
      [else (set! max max)]
      )
    )
  (print "highest probability: ") (println max)
  (list grade max)
)

;go through the network and get the beliefs for the schedule
(for ((course courses))
    (set! course (course-name course))
    (println course)
    (set! condition (to-condition-pairs '(((course-preq (list-ref (hash-ref catalog course) 0)) 0))))
    ;(print-beliefs course network condition)
    (set! belief (cons course (get-beliefs course network condition)))
    (set! cs-net (cons belief cs-net))
)

(define break-cond #f)
;;;need to rewrite the below loop to allow for a break condition 
;(for ((i [in-naturals 0])
;      #:break (eq? #t break-cond)

;go through the network and get the list of courses and what they depend on
(for ((belief cs-net))
    (define course (list-ref belief 0))
    (define cond (get-condition-names course network))
    (print course) (print " conditioned on: ") (println cond)
    ;(define beliefs (get-beliefs course network cs1101gradeh))

    ;(get-expected-grade cs2201beliefs)
)

;;;;;;; testing snippets
(define cond-name (get-condition-names (course-name (list-ref courses 3)) network))
;(print cond-name)
(define cs1101 (list-ref (get-condition-names (course-name (list-ref courses 3)) network) 0))
;(print cs1101)

;;;;assign the grade for cs1101 to 3. this version does not work properly (see last email to kate)
(define cs1101grade (to-condition-pairs (list (list cs1101 3))))
;(print cs1101grade)

;;;;assign the grade for cs1101 to 3, another version. this version works. 
(define cs1101gradeh (to-condition-pairs '((CS1101 3))))
;(print cs1101gradeh)
(define cs1101beliefs (get-beliefs cs1101 network cs1101grade))
;(print cs1101beliefs)
(define cs1101-prob-3 (belief-prob (list-ref cs1101beliefs (condition-pair-grade (list-ref cs1101grade 0)))))
;(print cs1101-prob-3)
;(print (condition-pair-grade (list-ref cs1101grade 0)))
(define condpair (to-condition-pairs '(((list-ref courses 0) 4))))
;(print condpair)
;(belief-conditioned-on (list-ref (list-ref cs-net 0) 1))
;(list-ref (list-ref cs-net 0) 0)

;;;this should only give the beliefs for CS2201 conditioned on CS1101=3, but doesnt
(print "beliefs with updated code: ")
(println "")
(print-beliefs 'CS2201 network cs1101grade)
(println "***")
;;;this should only give the beliefs for CS2201 conditioned on CS1101=3, but works because CS1101 is directly coded in above
;(print-beliefs 'CS2201 network cs1101gradeh)

;;;this is the set of beliefs for CS2201 conditioned on CS1101=3
(define cs2201beliefs (get-beliefs 'CS2201 network cs1101gradeh))

(get-expected-grade cs2201beliefs)

;(get-child-names cs1101 network)
;(belief-grade (list-ref cs1101beliefs 0))
;(belief-grade (list-ref cs1101beliefs 1))
;(belief-grade (list-ref cs1101beliefs 2))
    ;(println condition)
    ;(set! belief (get-beliefs course network condition))
    ;(println belief)
    ;(println "")


;(define condition (to-condition-pairs '(((course-preq (list-ref (hash-ref catalog cs1151) 0)) 0))))

;condition
;(list (condition-pair '(course-preq (list-ref (hash-ref catalog cs1151) 0)) 0))
;(set! belief (get-beliefs cs1151 network condition))
;belief


