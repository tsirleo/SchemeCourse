; заготовка "Доктора". Сентябрь 2024
; В учебных целях используется базовая версия Scheme
#lang scheme/base

(require racket/vector)
(require racket/list)

(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop-v2 name (vector)))

(define (doctor-driver-loop-v2 name history)
  (newline)
  (print '**)
  (let ((user-response (read)))
    (cond 
      ((equal? user-response '(goodbye))
       (printf "Goodbye, ~a!\n" name)
       (print '(see you next week)))
      (else 
       (let* ((new-history (update-history history user-response))
              (reply-strategy (choose-strategy (vector-length new-history)))
              (reply-text (generate-reply reply-strategy user-response new-history)))
         (print reply-text)
         (doctor-driver-loop-v2 name new-history))))))

(define (update-history history new-response)
  (if (vector-member new-response history)
      history
      (let ((new-history (vector-append (vector new-response) history)))
        (if (> (vector-length new-history) 7)
            (vector-take new-history 7)
            new-history))))

(define (choose-strategy history-length)
  (if (> history-length 0)
      (random 0 3)
      (random 0 2)))

(define (generate-reply strategy user-response history)
  (case strategy
    ((0) (hedge-answer))
    ((1) (qualifier-answer user-response))
    ((2) (history-answer history))))

(define (hedge-answer)
  (pick-random-vector '#((please go on)
                         (many people have the same sorts of feelings)
                         (many of my patients have told me the same thing)
                         (please continue)
                         (tell me more)
                         (i see)
                         (interesting))))

(define (qualifier-answer user-response)
  (append (pick-random-vector '#((you seem to think that)
                                 (you feel that)
                                 (why do you believe that)
                                 (why do you say that)
                                 (what makes you think that)
                                 (do you really believe that)
                                 (is that so)))
          (change-person user-response)))

(define (history-answer history)
  (let* ((random-index (random 0 (vector-length history)))
         (selected-response (vector-ref history random-index))
         (changed-response (change-person selected-response)))
    (append '(earlier you said that) changed-response)))

(define (reply user-response)
  (case (random 0 2)
    ((0) (hedge-answer))
    ((1) (qualifier-answer user-response))))

(define (pick-random-vector vctr)
  (vector-ref vctr (random 0 (vector-length vctr))))

(define (change-person phrase)
  (many-replace-v3 '((am are)
                     (are am)
                     (i you)
                     (me you)
                     (mine yours)
                     (my your)
                     (myself yourself)
                     (you i)
                     (your my)
                     (yours mine)
                     (yourself myself)
                     (we you)
                     (us you)
                     (our your)
                     (ours yours)
                     (ourselves yourselves)
                     (yourselves ourselves)
                     (shall will))
                   phrase))

(define (many-replace replacement-pairs lst)
  (cond ((null? lst) lst)
        (else (let ((pat-rep (assoc (car lst) replacement-pairs)))
                (cons (if pat-rep (cadr pat-rep)
                          (car lst))
                      (many-replace replacement-pairs (cdr lst)))))))

(define (many-replace-v2 replacement-pairs lst)
  (let loop ((remaining lst) (result '()))
    (if (null? remaining)
        (reverse result)
        (let ((pat-rep (assoc (car remaining) replacement-pairs)))
          (loop (cdr remaining)
                (cons (if pat-rep (cadr pat-rep) (car remaining))
                      result))))))

(define (many-replace-v3 replacement-pairs lst)
  (map (lambda (elem)
         (let ((pat-rep (assoc elem replacement-pairs)))
           (if pat-rep (cadr pat-rep) elem)))
       lst))

(define (vector-foldl f init vctr)
  (let ((length (vector-length vctr)))
    (let loop ((i 0) (result init))
      (if (= i length) result
          (loop (add1 i) (f i result (vector-ref vctr i)))))))

(define (vector-foldr f init vctr)
  (let ((length (vector-length vctr)))
    (let loop ((i (sub1 length)) (result init))
      (if (= i -1) result
          (loop (sub1 i) (f i result (vector-ref vctr i)))))))
          
          
; Helper function to mock random for deterministic testing
(define mock-random-values '())
(define (mock-random low high)
  (if (null? mock-random-values)
      (error "No more mock random values")
      (let ((value (car mock-random-values)))
        (set! mock-random-values (cdr mock-random-values))
        value)))

; Override random for testing
(define (random low high)
  (mock-random low high))

; Test helper to print pass/fail
(define (assert-equal test-name actual expected)
  (if (equal? actual expected)
      (printf "Test ~a: PASSED\n" test-name)
      (printf "Test ~a: FAILED (expected ~a, got ~a)\n" test-name expected actual)))

; Test hedge-answer
(define (test-hedge-answer)
  (set! mock-random-values '(0 4 6))
  (assert-equal "hedge-answer-1" (hedge-answer) '(please go on))
  (assert-equal "hedge-answer-2" (hedge-answer) '(tell me more))
  (assert-equal "hedge-answer-3" (hedge-answer) '(interesting)))

; Test qualifier-answer
(define (test-qualifier-answer)
  (set! mock-random-values '(0))
  (assert-equal "qualifier-answer-1"
                (qualifier-answer '(i feel sad))
                '(you seem to think that you feel sad)))

; Test many-replace
(define (test-many-replace)
  (let ((replacements '((i you) (my your) (am are)))
        (input '(i am sad about my life)))
    (assert-equal "many-replace-1"
                  (many-replace replacements input)
                  '(you are sad about your life))))

; Test many-replace-v2
(define (test-many-replace-v2)
  (let ((replacements '((i you) (my your) (am are)))
        (input '(i am sad about my life)))
    (assert-equal "many-replace-v2-1"
                  (many-replace-v2 replacements input)
                  '(you are sad about your life))))

; Test many-replace-v3
(define (test-many-replace-v3)
  (let ((replacements '((i you) (my your) (am are)))
        (input '(i am sad about my life)))
    (assert-equal "many-replace-v3-1"
                  (many-replace-v3 replacements input)
                  '(you are sad about your life))))

; Test change-person
(define (test-change-person)
  (assert-equal "change-person-1"
                (change-person '(i am sad about my life))
                '(you are sad about your life)))

; Test update-history
(define (test-update-history)
  (let ((h1 (vector))
        (h2 (vector '(i am sad)))
        (h3 (vector '(i am sad) '(you are mean))))
    (assert-equal "update-history-1"
                  (update-history h1 '(i am sad))
                  (vector '(i am sad)))
    (assert-equal "update-history-2"
                  (update-history h2 '(i am sad))
                  h2)
    (assert-equal "update-history-3"
                  (update-history h3 '(my life is hard))
                  (vector '(my life is hard) '(i am sad) '(you are mean)))
    (let ((h4 (vector '(r7) '(r6) '(r5) '(r4) '(r3) '(r2) '(r1))))
      (assert-equal "update-history-4"
                    (update-history h4 '(r8))
                    (vector '(r8) '(r7) '(r6) '(r5) '(r4) '(r3) '(r2))))))

; Test choose-strategy
(define (test-choose-strategy)
  (set! mock-random-values '(0 1 2 0))
  (assert-equal "choose-strategy-1" (choose-strategy 0) 0)
  (assert-equal "choose-strategy-2" (choose-strategy 0) 1)
  (assert-equal "choose-strategy-3" (choose-strategy 1) 2)
  (assert-equal "choose-strategy-4" (choose-strategy 1) 0))

; Test history-answer
(define (test-history-answer)
  (set! mock-random-values '(1 0))
  (let ((history (vector '(i am happy) '(i am sad))))
    (assert-equal "history-answer-1"
                  (history-answer history)
                  '(earlier you said that you are sad))
    (assert-equal "history-answer-2"
                  (history-answer history)
                  '(earlier you said that you are happy))))

; Test generate-reply
(define (test-generate-reply)
  (set! mock-random-values '(0 0 0)) ; For hedge-answer, qualifier-answer, history-answer
  (let ((history (vector '(i am sad))))
    (assert-equal "generate-reply-1" (generate-reply 0 '(i feel bad) history) '(please go on))
    (assert-equal "generate-reply-2"
                  (generate-reply 1 '(i feel bad) history)
                  '(you seem to think that you feel bad))
    (assert-equal "generate-reply-3"
                  (generate-reply 2 '(i feel bad) history)
                  '(earlier you said that you are sad))))

; Run all tests
(define (run-tests)
  (printf "=== Running Tests ===\n")
  (test-hedge-answer)
  (test-qualifier-answer)
  (test-many-replace)
  (test-many-replace-v2)
  (test-many-replace-v3)
  (test-change-person)
  (test-update-history)
  (test-choose-strategy)
  (test-history-answer)
  (test-generate-reply)
  (printf "=== All tests completed ===\n")
)

(run-tests)
