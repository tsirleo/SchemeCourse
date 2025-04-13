#lang racket

;; Конструктор потока
(define (stream-cons head tail-thunk)
  (cons head tail-thunk))

(define (stream-first s)
  (car s))

(define (stream-rest s)
  (force (cdr s)))

;; Функция создания бесконечного потока степеней заданного числа
(define (stream-of-powers base)
  (define (iter n)
    (stream-cons n (delay (iter (* n base)))))
  (iter 1)
)

;; Функция слияния двух упорядоченных потоков без дубликатов
(define (merge-streams s1 s2)
  (let ((a (stream-first s1))
        (b (stream-first s2)))
    (cond 
      ((< a b)
        (stream-cons a (delay (merge-streams (stream-rest s1) s2))))
      ((< b a)
        (stream-cons b (delay (merge-streams s1 (stream-rest s2)))))
      (else ; если a = b, выводим один раз и пропускаем дубликат
        (stream-cons a (delay (merge-streams (stream-rest s1) (stream-rest s2)))))
    )
  )
)

;; Создание потоков степеней 3 и 5
(define powers-3 (stream-of-powers 3))
(define powers-5 (stream-of-powers 5))

;; Объединение потоков в один, упорядоченный по возрастанию
(define merged-powers (merge-streams powers-3 powers-5))

;; Функция преобразования потока в список
(define (stream->list s n)
  (if (= n 0)
      '()
      (cons (stream-first s)
            (stream->list (stream-rest s) (sub1 n)))
  )
)

;; Вывод первых 10 элементов результирующего потока
(displayln (stream->list merged-powers 10))
