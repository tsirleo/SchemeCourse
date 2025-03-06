#lang racket

; (define (list-fib-squares n)
;   (map 
;     (lambda (x)
;       (let ((temp (fib x))) (* temp temp))
;     )
;     (enumerate-interval 1 n)
;   )
; )

(define (list-fib-squares-a n)
  (define (loop k a b acc)
    (if (> k n)
      (reverse acc)
      (loop (+ k 1) b (+ a b) (cons (* b b) acc))
    )
  )
  (if (<= n 0)
    '()
    (loop 1 0 1 '())
  )
)

(define (list-fib-squares-b n)
  (reverse
    (caddr
      (foldl 
        (lambda (_ state)
          (let 
            ((a (car state))
              (b (cadr state))
              (acc (caddr state))
            )
            (list 
              b
              (+ a b)
              (cons (* b b) acc)
            )
          )
        )
        '(0 1 ())
        (build-list n values)
      )
    )
  )
)

(list-fib-squares-a 6)
(list-fib-squares-b 6)