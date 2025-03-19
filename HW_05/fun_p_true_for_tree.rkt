#lang racket

; vector-based tree functions
(define empty-tree #())

(define make-tree vector)

(define (tree-data tree) (vector-ref tree 0))

(define (tree-left tree) (vector-ref tree 1))

(define (tree-right tree) (vector-ref tree 2))

(define (empty-tree? t) (equal? t #()))


; solution function cps
(define (for-all-tree fun-p tree)
  (let loop ((tree tree) (cont (lambda (result) result)))
    (if (empty-tree? tree)
      (cont #t)
      (let (
            (value (tree-data tree))
            (left  (tree-left tree))
            (right (tree-right tree))
           )
        (if (not (fun-p value))
          (cont #f)
          (loop left
            (lambda (left-result)
              (if (not left-result)
                  (cont #f)
                  (loop right cont)
              )
            )
          )
        )
      )
    )
  )
)


; solution function call/cc
(define (for-all-tree-call/cc fun-p tree)
  (call/cc
    (lambda (exit)
      (let loop ((tree tree))
        (if (empty-tree? tree)
          #t
          (let (
               (value (tree-data tree))
               (left  (tree-left tree))
               (right (tree-right tree))
              )
            (if (not (fun-p value))
              (exit #f)
              (and (loop left) (loop right))
            )
          )
        )
      )
    )
  )
)


(for-all-tree odd? #()) ; -> #t
(for-all-tree odd? #(1 #() #())) ; -> #t
(for-all-tree odd? #(2 #() #())) ; -> #f
(for-all-tree odd? #(3 #(1 #() #()) #(5 #() #()))) ; -> #t
(printf "\n")
(for-all-tree-call/cc odd? #()) ; -> #t
(for-all-tree-call/cc odd? #(1 #() #())) ; -> #t
(for-all-tree-call/cc odd? #(2 #() #())) ; -> #f
(for-all-tree-call/cc odd? #(3 #(1 #() #()) #(5 #() #()))) ; -> #t
