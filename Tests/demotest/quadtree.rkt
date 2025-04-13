#lang racket

(define (taskII t s)
  (cond
    ((equal? t 1) s)
    ((equal? t 0) 0)
    (else
      (let ((s/4 (/ s 4)))
        (+ (taskII (vector-ref t 0) s/4)
          (taskII (vector-ref t 1) s/4)
          (taskII (vector-ref t 2) s/4)
          (taskII (vector-ref t 3) s/4)
        )
      )
    )
  )
)
      

(taskII 1 16) ; => 16 
(taskII 0 16) ; => 0 
(taskII #(1 0 0 #(1 1 1 0)) 16) ; => 7 
(taskII #(1 0 #(0 1 0 1) 0) 64) ; => 24 
(taskII #(1 1 0 0) 4) ; => 2
