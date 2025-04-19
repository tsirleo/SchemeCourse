#lang racket

(define (fun-v n tree)
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
    (call/cc
      (lambda (return)
        (let check-tree ((k n) (t tree))
          (cond
            ((not (and (vector? t) (= (vector-length t) 3)))
              (return #f))
            ((<= k 1)
              (if (and
                    (= (vector-ref t 0) (vector-ref memo k))
                    (vector? (vector-ref t 1)) (= (vector-length (vector-ref t 1)) 0)
                    (vector? (vector-ref t 2)) (= (vector-length (vector-ref t 2)) 0)
                  )
                #t
                (return #f)
              )
            )
            (else
              (if (not (= (vector-ref t 0) (vector-ref memo k)))
                (return #f)
                (and
                  (check-tree (- k 1) (vector-ref t 1))
                  (check-tree (- k 2) (vector-ref t 2))
                )
              )
            )
          )
        )
      )
    )
  )
)
                 

(fun-v 0 #(0 #() #())) ; => #t  
(fun-v 0 #(1 #() #())) ; => #f
(fun-v 2 #(1 #(1 #() #()) #(0 #() #()))) ; => #t
