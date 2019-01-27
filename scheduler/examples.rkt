#lang racket

(require "read-in.rkt")
(require "schedule-tester.rkt")

;; check some examples
;;(define catalog (preq-list-parser "preq-lists/all-cs-courses.txt"))
;;(define list-of-entries (hash-ref catalog 'ASTR3800))
;;(define all-possible-preq-lists (map course-preq list-of-entries))

;;(define conditions (to-condition-pairs '((CS2212 4)(CS1101 4)(CS3270 4))))

(define network (bayesian-network-parser "bayesian-networks/all-cs-courses.txt"))

;;(get-beliefs 'CS3251 network conditions)