#lang racket

(define (factorial n)
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))
  )
)

;; Создание списка первых факториалов
(define factorials
  (let loop ((n 0) (acc '()))
    (let ((fact (factorial n)))
      (if (> fact 1000000)
        (reverse acc)
        (loop (+ n 1) (cons fact acc))
      )
    )
  )
)

;; Порождающая функция потока nonfactorials
(define (make-nonfactorials)
  (let ((current 1))
    (define (next)
      (let loop ()
        (set! current (+ current 1))
        (if (member current factorials)
          (loop)
          current
        )
      )
    )
    next
  )
)

(define nonfactorials
  (let ((gen (make-nonfactorials)))
    (define (stream)
      (stream-cons (gen) (stream)))
    (stream)
  )
)

(define (stream-take stream n)
  (if (<= n 0)
    '()
    (cons (stream-first stream)
      (stream-take (stream-rest stream) (- n 1))
    )
  )
)

(displayln (stream-take nonfactorials 21))
