#lang racket

(define (fun-iv-cps n tree cc)
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
    (let check-tree ((k n) (t tree) (cont cc))
      (cond
        ((not (and (vector? t) (= (vector-length t) 3)))
          (cont #f))
        ((<= k 1)
          (cont 
            (and
              (= (vector-ref t 0) (vector-ref memo k))
              (vector? (vector-ref t 1)) (= (vector-length (vector-ref t 1)) 0)
              (vector? (vector-ref t 2)) (= (vector-length (vector-ref t 2)) 0)
            )
          )
        )
        (else
          (if (not (= (vector-ref t 0) (vector-ref memo k)))
            (cont #f)
            (check-tree (- k 1)
              (vector-ref t 1)
              (lambda (left-result)
                (if (not left-result)
                  (cont #f)
                  (check-tree 
                    (- k 2)
                    (vector-ref t 2)
                    cont
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


(fun-iv-cps 0 #(0 #() #()) not) ; => #f  
(fun-iv-cps 0 #(1 #() #()) not) ; => #t
(fun-iv-cps 2 #(1 #(1 #() #()) #(0 #() #())) not) ; => #f
