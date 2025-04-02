#lang racket

(define (sum-divisors x)
  (if (= x 1)
    1
    (let loop ((i 1) (sum 0))
      (cond
        ((> i (sqrt x)) sum)
        ((= (remainder x i) 0)
          (if (= i (/ x i))
            (loop (+ i 1) (+ sum i))
            (loop (+ i 1) (+ sum i (/ x i)))
          )
        )
        (else (loop (+ i 1) sum))
      )
    )
  )
)

(define (deficient? x)
  (< (sum-divisors x) (* 2 x))
)

(define (odd-deficient n)
  (let loop ((current 1) (count 0))
    (cond
      ((= count n) (- current 2))
      ((and (odd? current) (deficient? current))
        (loop (+ current 2) (+ count 1)))
      (else (loop (+ current 2) count))
    )
  )
)

(define deficient-memo (make-hash '((1 . #t))))

(define (memo-deficient? x)
  (let ((memo-result (hash-ref deficient-memo x #f)))
    (if memo-result
      memo-result
      (let ((result (< (sum-divisors x) (* 2 x))))
        (hash-set! deficient-memo x result)
        result
      )
    )
  )
)

(define (memo-odd-deficient n)
  (let loop ((current 1) (count 0))
    (cond
      ((= count n) (- current 2))
      ((and (odd? current) (memo-deficient? current))
        (loop (+ current 2) (+ count 1)))
      (else (loop (+ current 2) count))
    )
  )
)
      
;; Тесты для sum-divisors
(displayln "Тесты для sum-divisors:")
(displayln (equal? (sum-divisors 1) 1))    ; 1 → 1
(displayln (equal? (sum-divisors 3) 4))    ; 1+3 = 4
(displayln (equal? (sum-divisors 9) 13))   ; 1+3+9 = 13
(displayln (equal? (sum-divisors 15) 24))  ; 1+3+5+15 = 24

;; Тесты для deficient?
(displayln "\nТесты для deficient?:")
(displayln (equal? (deficient? 1) #t))     ; 1 < 2
(displayln (equal? (deficient? 3) #t))     ; 4 < 6
(displayln (equal? (deficient? 9) #t))     ; 13 < 18
(displayln (equal? (deficient? 15) #f))    ; 24 = 24 (не недостаточное)

;; Тесты для odd-deficient
(displayln "\nТесты для odd-deficient:")
(displayln (equal? (odd-deficient 1) 1))   ; 1-й элемент: 1
(displayln (equal? (odd-deficient 2) 3))   ; 2-й элемент: 3
(displayln (equal? (odd-deficient 5) 9))   ; 5-й элемент: 9
(displayln (equal? (odd-deficient 10) 19)) ; 10-й элемент: 19

;; Тесты для memo-odd-deficient
(displayln "\nТесты для memo-odd-deficient:")
(displayln (equal? (memo-odd-deficient 1) 1))
(displayln (equal? (memo-odd-deficient 2) 3))
(displayln (equal? (memo-odd-deficient 5) 9))
(displayln (equal? (memo-odd-deficient 10) 19))

;; Тест производительности
(displayln "\nТест производительности для n=10000:")
;; Первый вызов - мемоизация только начинает заполняться
(display "Обычная версия: ")
(time (odd-deficient 10000))

(display "Мемоизированная версия (первый вызов): ")
(time (memo-odd-deficient 10000))

;; Повторный вызов - используем кэш
(display "Мемоизированная версия (повторный вызов): ")
(time (memo-odd-deficient 10000))

;; Последовательные вызовы - реальное преимущество
(displayln "\nТест последовательных вызовов (n от 10000 до 10005):")

(display "Обычная версия: ")
(time (begin 
        (odd-deficient 10000)
        (odd-deficient 10001)
        (odd-deficient 10002)
        (odd-deficient 10003)
        (odd-deficient 10004)
        (odd-deficient 10005)))

(display "Мемоизированная версия: ")
(time (begin
        (memo-odd-deficient 10000)
        (memo-odd-deficient 10001)
        (memo-odd-deficient 10002)
        (memo-odd-deficient 10003)
        (memo-odd-deficient 10004)
        (memo-odd-deficient 10005)))

