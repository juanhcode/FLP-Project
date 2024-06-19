#lang racket/base

(require rackunit "../proyecto.rkt")

;Test 1: 
(define exp1
  (scan&parse
     "true"
  )
)
(define expected-exp1 #t)


;Test 2: 
(define exp2
  (scan&parse
     "false"
  )
)
(define expected-exp2 #f)

(check-equal? (evaluar-programa exp2) expected-exp2)
