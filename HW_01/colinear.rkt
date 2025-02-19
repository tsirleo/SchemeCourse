#lang racket

(define (colinear? x1 y1 z1 x2 y2 z2)
  (cond
    ((and (= x1 0) (= y1 0) (= z1 0)) #t)
    ((and (= x2 0) (= y2 0) (= z2 0)) #t)
    (else
      (and 
        (= (- (* y1 z2) (* y2 z1)) 0)
        (= (- (* x1 z2) (* x2 z1)) 0)
        (= (- (* x1 y2) (* x2 y1)) 0)
      )
    )
  )
)

