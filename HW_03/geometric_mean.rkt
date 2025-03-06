#lang racket

(define (task3 lst)
  (define (vecLength vec)
    (sqrt (foldl + 0 (map (lambda (x) (expt x 2)) vec)))
  )
  
  (define lenList (map vecLength lst))
  (define n (length lenList))
  
  (expt (foldl * 1 lenList) (/ 1 n))
)


(task3 (list (list 3 4) (list 2 2 1) (list 6 8) (list 4320 3240)))