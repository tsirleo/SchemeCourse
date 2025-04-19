#lang racket

(define (adjacent-map fun lst)
  (let ((rest (cdr lst))) 
    (reverse
      (cdr 
        (foldl 
          (lambda (x acc)
            (cons x (cons (fun (car acc) x) (cdr acc))))
          (cons (car lst) '())
          rest
        )
      )
    )
  )
)
                  
                  
(adjacent-map * '(1 2 3 4 5 6)) ; => '(2 6 12 20 30)
(adjacent-map < '(1 2 1 3 4 3)) ; => '(#t #f #t #t #f)
