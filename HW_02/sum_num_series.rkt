#lang racket

(define (task2a n)
  (if (and (integer? n) (> n 0))
    (let loop ((i 1) (sum 0))
      (if (> i n)
          sum
          (loop (+ i 1) (+ sum (* i (+ i 2))))
      )
    )
    0
  )
)

(define (task2b n)
  (if (and (integer? n) (> n 0))
    (if (= n 1)
        3
        (+ (* n (+ n 2)) (task2b (- n 1)))
    )
    0
  )
)

(define (task2c n)
  (if (and (integer? n) (> n 0))
    (/ (* n (+ n 1) (+ (* 2 n) 7)) 6)
    0
  )
)
      
(task2a 7)
(task2b 7)
(task2c 7)
