#lang racket

(define-syntax left-rot!
  (syntax-rules ()
    ((left-rot! m0 m1 ...)
      (let ([tmp m0])
        (shift-left! (m0 m1 ...) (m1 ... tmp))
      )
    )
  )
)

(define-syntax shift-left!
  (syntax-rules ()
    ((shift-left! () ()) (void))
    ((shift-left! (x) (y)) (set! x y))
    ((shift-left! (x xs ...) (y ys ...))
      (begin
        (set! x y)
        (shift-left! (xs ...) (ys ...))
      )
    )
  )
)

(let ((a 3) (b 2) (c 1)) (begin (left-rot! a b c) (/ a b c))) ; => 2/3
(let ([x 10] [y 20]) (begin (left-rot! x y) (list x y))) ; => '(20 10)
(let ([a 3] [b 2] [c 1]) (begin (left-rot! a b c) (list a b c))) ; => '(2 1 3)
(let ([p "a"] [q "b"] [r "c"] [s "d"]) (begin (left-rot! p q r s) (list p q r s))) ; => '("b" "c" "d" "a")
(let ([a 1] [b 2] [c 3] [d 4]) (begin (left-rot! a b c d) (left-rot! a b c d) (+ a b c d))) ; => 10

