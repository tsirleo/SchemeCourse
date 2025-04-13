#lang racket

(define (filter1 f lst)
  (reverse (foldl (lambda (x y) (if (f x) (cons x y) y)) null lst)))

(define (filter2 f lst)
  (foldr (lambda (x y) (if (f x) (cons x y) y)) null lst))
