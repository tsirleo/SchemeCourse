#lang racket

(define (is-prime? n)
  (cond
    ((< n 2) #f)
    (else
      (let loop ((i 2))
        (cond
          ((> (* i i) n) #t)
          ((= (remainder n i) 0) #f)
          (else (loop (+ i 1)))
        )
      )
    )
  )
)

(define taskV
  (let ((cache (make-hash)))
    (lambda (n)
      (cond
        ((< n 1) (error "n должно быть положительным"))
        ((hash-ref cache n #f) => identity)
        (else
          (let loop ((k 2) (count 0) (primes '()))
            (if (>= count n)
              (let ((result (car primes)))
               (hash-set! cache n result)
               result
              )
              (let ((candidate (sub1 (expt 2 k))))
                (if (is-prime? candidate)
                  (loop (+ k 1) (+ count 1) (cons candidate primes))
                  (loop (+ k 1) count primes)
                )
              )
            )
          )
        )
      )
    )
  )
)

(taskV 1) ; => 3
(taskV 2) ; => 7
(taskV 3) ; => 31
(taskV 4) ; => 127
(taskV 5) ; => 8191
(taskV 6) ; => 131071
