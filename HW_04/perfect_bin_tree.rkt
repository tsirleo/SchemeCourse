#lang racket

; vector-based tree functions
(define empty-tree #())

(define make-tree vector)

(define (tree-data tree) (vector-ref tree 0))

(define (tree-left tree) (vector-ref tree 1))

(define (tree-right tree) (vector-ref tree 2))

(define (empty-tree? t) (equal? t #()))


; solution function
(define (task-4 tree h)
  (cond
    ((empty-tree? tree) (= h 0))
    ((= h 0) #f)
    (else
      (and 
        (vector? tree)
        (= (vector-length tree) 3)
        (task-4 (tree-left tree) (- h 1))
        (task-4 (tree-right tree) (- h 1))
      )
    )
  )
)


(task-4 #() 0) ; -> #t
(task-4 #(1 #() #()) 1) ; -> #t
(task-4 #() 1) ; -> #f
(task-4 #(1 #(2 #() #()) #(3 #() #())) 2) ; -> #t
(task-4 #(1 #(2 #() #(4 #() #())) #(3 #() #())) 2) ; -> #f
