#lang racket

; vector-based tree functions
(define empty-tree #())

(define make-tree vector)

(define (tree-data tree) (vector-ref tree 0))

(define (tree-left tree) (vector-ref tree 1))

(define (tree-right tree) (vector-ref tree 2))

(define (empty-tree? t) (equal? t #()))


; solution function
(define (task-5 tree h)
  (call/cc
    (lambda (exit)
      (letrec
        ((check
          (lambda (t height)
            (cond
              ((empty-tree? t)
                (if (= height 0)
                    #t
                    (exit #f)
                )
              )
              ((= height 0)
                (exit #f)
              )
              [else
                (if (and (vector? t) (= (vector-length t) 3))
                    (let (
                          (left-ok (check (tree-left t) (- height 1)))
                          (right-ok (check (tree-right t) (- height 1)))
                        )
                        (and left-ok right-ok)
                    )
                    (exit #f)
                )
              ]
            )
          )
        ))
        (check tree h)
      )
    )
  )
)
       
(task-5 #() 0) ; -> #t
(task-5 #(1 #() #()) 1) ; -> #t
(task-5 #() 1) ; -> #f
(task-5 #(1 #(2 #() #()) #(3 #() #())) 2) ; -> #t
(task-5 #(1 #(2 #() #(4 #() #())) #(3 #() #())) 2) ; -> #f

