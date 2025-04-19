#lang racket

(define (chunk-by fun lst)
  (foldr
    (lambda (x acc)
      (if (and (pair? acc)
            (fun x (caar acc)))
        (cons (cons x (car acc)) (cdr acc))
        (cons (list x) acc)
      )
    )
    '()
    lst
  )
)


(chunk-by eq? '(1 1 3 2 2)) ; => ((1 1) (3) (2 2))
(chunk-by <  '(1 2 1 3 4 3)) ; => ((1 2) (1 3 4) (3))
