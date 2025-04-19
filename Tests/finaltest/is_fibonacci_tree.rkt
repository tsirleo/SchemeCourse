#lang racket

(define (fun-iii n tree)
  (let ((memo (make-vector (+ n 1))))
    (vector-set! memo 0 0)
    (when (>= n 1)
      (vector-set! memo 1 1))
    (let loop ((i 2))
      (unless (> i n)
        (vector-set! memo i (+ (vector-ref memo (- i 1)) (vector-ref memo (- i 2))))
        (loop (+ i 1))
      )
    )
    (let check-tree ((k n) (t tree))
      (cond
        ((not (and (vector? t) (= (vector-length t) 3))) #f)
        ((<= k 1)
          (and
            (= (vector-ref t 0) (vector-ref memo k))
            (vector? (vector-ref t 1)) (= (vector-length (vector-ref t 1)) 0)
            (vector? (vector-ref t 2)) (= (vector-length (vector-ref t 2)) 0)
          )
        )
        (else
          (and
            (= (vector-ref t 0) (vector-ref memo k))
            (check-tree (- k 1) (vector-ref t 1))
            (check-tree (- k 2) (vector-ref t 2))
          )
        )
      )
    )
  )
)


(fun-iii 0 #(0 #() #())) ; => #t
(fun-iii 0 #(1 #() #())) ; => #f
(fun-iii 2 #(1 #(1 #() #()) #(0 #() #()))) ; => #t
