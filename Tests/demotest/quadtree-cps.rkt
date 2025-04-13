#lang racket

(define (taskIII-cps t s cc)
  (cond
    ((equal? t 1) (cc s))
    ((equal? t 0) (cc 0))
    (else
      (let ((s/4 (/ s 4)))
        (taskIII-cps (vector-ref t 0) s/4
          (lambda (area0)
            (taskIII-cps (vector-ref t 1) s/4
              (lambda (area1)
                (taskIII-cps (vector-ref t 2) s/4
                  (lambda (area2)
                    (taskIII-cps (vector-ref t 3) s/4
                      (lambda (area3)
                        (cc (+ area0 area1 area2 area3))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)
      

(taskIII-cps 1 16 identity) ; => 16 
(taskIII-cps 0 16 identity) ; => 0 
(taskIII-cps #(1 0 0 #(1 1 1 0)) 16 identity) ; => 7 
(taskIII-cps #(1 0 #(0 1 0 1) 0) 64 identity) ; => 24 
(taskIII-cps #(1 1 0 0) 4 identity) ; => 2
