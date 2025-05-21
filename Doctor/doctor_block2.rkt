#lang scheme/base

(require racket/vector)
(require racket/list)

(define (ask-patient-name)
  (begin
    (println '(next!))
    (println '(who are you?))
    (print '**)
    (car (read))))

(define keywords-structure '#(
  #( #(depressed suicide exams university)
     #( (when you feel depressed go out for ice cream)
        (depression is a disease that can be treated)
        (when you feel * try to find something positive)
        (* can be overwhelming but you can manage it) ) )
  #( #(mother father parents brother sister uncle aunt grandma grandpa)
     #( (tell me more about your * i want to know all about your *)
        (why do you feel that way about your * ?)
        (how does your * feel about that?)
        (have you talked to your * about this?) ) )
  #( #(university scheme lections)
     #( (your education is important)
        (how much time do you spend on your studies ?) ) )
  #( #(happy sad angry excited)
     #( (its good to feel *)
        (why do you feel * ?)
        (tell me more about feeling *) ) )
  #( #(reading writing painting music)
     #( (do you enjoy * ?)
        (how often do you * ?)
        (* is a great hobby) ) )
))

(define all-keywords
  (remove-duplicates
   (apply append
          (map (lambda (group) (vector->list (vector-ref group 0)))
               (vector->list keywords-structure)))))

(define (visit-doctor-v2 stop-word max-patients)
  (let loop ((patients-served 0))
    (if (= patients-served max-patients)
        (println '(time to go home))
        (let ((name (ask-patient-name)))
          (if (equal? name stop-word)
              (println '(time to go home))
              (begin
                (printf "Hello, ~a!\n" name)
                (print '(what seems to be the trouble?))
                (doctor-driver-loop-v2 name (vector))
                (loop (+ patients-served 1))))))))

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
              (reply-strategy (choose-strategy (vector-length new-history) user-response))
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

(define (has-keywords? user-response)
  (ormap (lambda (word) (not (not (member word all-keywords)))) user-response))

(define (get-keywords-from-input input)
  (filter (lambda (word) (member word all-keywords)) input))

(define (select-keyword input)
  (let ((keywords-in-input (get-keywords-from-input input)))
    (list-ref keywords-in-input (random 0 (length keywords-in-input)))))

(define (get-groups-for-keyword keyword)
  (filter (lambda (group)
            (member keyword (vector->list (vector-ref group 0))))
          (vector->list keywords-structure)))

(define (get-templates-for-keyword keyword)
  (apply append
         (map (lambda (group) (vector->list (vector-ref group 1)))
              (get-groups-for-keyword keyword))))

(define (pick-random-list lst)
  (list-ref lst (random 0 (length lst))))

(define (replace-star template keyword)
  (many-replace `((* ,keyword)) template))

(define (generate-keyword-reply input)
  (let* ((keyword (select-keyword input))
         (templates (get-templates-for-keyword keyword))
         (template (pick-random-list templates))
         (reply (replace-star template keyword)))
    reply))

(define (choose-strategy history-length user-response)
  (let ((has-keywords (has-keywords? user-response)))
    (let ((available-strategies
           (cond
             ((and (= history-length 0) (not has-keywords)) '(0 1))
             ((and (> history-length 0) (not has-keywords)) '(0 1 2))
             ((and (= history-length 0) has-keywords) '(0 1 3))
             (else '(0 1 2 3)))))
      (pick-random-list available-strategies))))

(define (generate-reply strategy user-response history)
  (case strategy
    ((0) (hedge-answer))
    ((1) (qualifier-answer user-response))
    ((2) (history-answer history))
    ((3) (generate-keyword-reply user-response))))

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

;; Helper function to mock random for deterministic testing
(define mock-random-values '())
(define (mock-random low high)
  (if (null? mock-random-values)
      (error "No more mock random values")
      (let ((value (car mock-random-values)))
        (set! mock-random-values (cdr mock-random-values))
        value)))

;; Override random for testing
(define (random low high)
  (mock-random low high))

;; Test helper to print pass/fail
(define (assert-equal test-name actual expected)
  (if (equal? actual expected)
      (printf "Test ~a: PASSED\n" test-name)
      (printf "Test ~a: FAILED (expected ~a, got ~a)\n" test-name expected actual)))

;; Test ask-patient-name (interactive, so describe expected behavior)
(define (test-ask-patient-name)
  (printf "Test ask-patient-name: Expecting (next!) (who are you?) **, input e.g., (hal) returns 'hal\n"))

;; Test has-keywords?
(define (test-has-keywords)
  (assert-equal "has-keywords-1" (has-keywords? '(i feel depressed about scheme)) #t)
  (assert-equal "has-keywords-2" (has-keywords? '(i like to eat)) #f))

;; Test get-keywords-from-input
(define (test-get-keywords-from-input)
  (assert-equal "get-keywords-from-input-1"
                (get-keywords-from-input '(i feel depressed about scheme scheme))
                '(depressed scheme scheme)))

;; Test select-keyword
(define (test-select-keyword)
  (set! mock-random-values '(0 1))
  (assert-equal "select-keyword-1"
                (select-keyword '(i feel depressed about scheme scheme))
                'depressed)
  (assert-equal "select-keyword-2"
                (select-keyword '(i feel depressed about scheme scheme))
                'scheme))

;; Test get-groups-for-keyword
(define (test-get-groups-for-keyword)
  (assert-equal "get-groups-for-keyword-1"
                (length (get-groups-for-keyword 'university))
                2)
  (assert-equal "get-groups-for-keyword-2"
                (length (get-groups-for-keyword 'mother))
                1))

;; Test get-templates-for-keyword
(define (test-get-templates-for-keyword)
  (assert-equal "get-templates-for-keyword-1"
                (length (get-templates-for-keyword 'university))
                6)
  (assert-equal "get-templates-for-keyword-2"
                (length (get-templates-for-keyword 'happy))
                3))

;; Test pick-random-list
(define (test-pick-random-list)
  (set! mock-random-values '(0 2))
  (assert-equal "pick-random-list-1" (pick-random-list '(a b c)) 'a)
  (assert-equal "pick-random-list-2" (pick-random-list '(a b c)) 'c))

;; Test replace-star
(define (test-replace-star)
  (assert-equal "replace-star-1"
                (replace-star '(tell me about your * please) 'mother)
                '(tell me about your mother please))
  (assert-equal "replace-star-2"
                (replace-star '(why do you feel * ?) 'sad)
                '(why do you feel sad ?)))

;; Test generate-keyword-reply
(define (test-generate-keyword-reply)
  (set! mock-random-values '(0 0)) ; First for select-keyword, second for pick-random-list
  (assert-equal "generate-keyword-reply-1"
                (generate-keyword-reply '(i feel depressed about scheme))
                '(when you feel depressed go out for ice cream))
  (set! mock-random-values '(1 1)) ; Select 'scheme', pick second template from university group
  (assert-equal "generate-keyword-reply-2"
                (generate-keyword-reply '(i feel depressed about scheme))
                '(how much time do you spend on your studies ?)))

;; Test pick-random-vector
(define (test-pick-random-vector)
  (set! mock-random-values '(0 4))
  (assert-equal "pick-random-vector-1"
                (pick-random-vector '#(x y z w v))
                'x)
  (assert-equal "pick-random-vector-2"
                (pick-random-vector '#(x y z w v))
                'v))

;; Test hedge-answer
(define (test-hedge-answer)
  (set! mock-random-values '(0 4 6))
  (assert-equal "hedge-answer-1" (hedge-answer) '(please go on))
  (assert-equal "hedge-answer-2" (hedge-answer) '(tell me more))
  (assert-equal "hedge-answer-3" (hedge-answer) '(interesting)))

;; Test qualifier-answer
(define (test-qualifier-answer)
  (set! mock-random-values '(0))
  (assert-equal "qualifier-answer-1"
                (qualifier-answer '(i feel sad))
                '(you seem to think that you feel sad)))

;; Test many-replace
(define (test-many-replace)
  (let ((replacements '((i you) (my your) (am are)))
        (input '(i am sad about my life)))
    (assert-equal "many-replace-1"
                  (many-replace replacements input)
                  '(you are sad about your life))))

;; Test many-replace-v2
(define (test-many-replace-v2)
  (let ((replacements '((i you) (my your) (am are)))
        (input '(i am sad about my life)))
    (assert-equal "many-replace-v2-1"
                  (many-replace-v2 replacements input)
                  '(you are sad about your life))))

;; Test many-replace-v3
(define (test-many-replace-v3)
  (let ((replacements '((i you) (my your) (am are)))
        (input '(i am sad about my life)))
    (assert-equal "many-replace-v3-1"
                  (many-replace-v3 replacements input)
                  '(you are sad about your life))))

;; Test change-person
(define (test-change-person)
  (assert-equal "change-person-1"
                (change-person '(i am sad about my life))
                '(you are sad about your life)))

;; Test update-history
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

;; Test history-answer
(define (test-history-answer)
  (set! mock-random-values '(1 0))
  (let ((history (vector '(i am happy) '(i am sad))))
    (assert-equal "history-answer-1"
                  (history-answer history)
                  '(earlier you said that you are sad))
    (assert-equal "history-answer-2"
                  (history-answer history)
                  '(earlier you said that you are happy))))

;; Test choose-strategy
(define (test-choose-strategy)
  (set! mock-random-values '(0 1 2 3))
  (assert-equal "choose-strategy-1" (choose-strategy 0 '(i like to eat)) 0)
  (assert-equal "choose-strategy-2" (choose-strategy 0 '(i like to eat)) 1)
  (assert-equal "choose-strategy-3" (choose-strategy 1 '(i like to eat)) 2)
  (assert-equal "choose-strategy-4" (choose-strategy 1 '(i feel depressed)) 3))

;; Test generate-reply
(define (test-generate-reply)
  (set! mock-random-values '(0 0 0 0 0)) ; For hedge, qualifier, history, select-keyword, pick-random-list
  (let ((history (vector '(i am sad))))
    (assert-equal "generate-reply-1" (generate-reply 0 '(i feel bad) history) '(please go on))
    (assert-equal "generate-reply-2"
                  (generate-reply 1 '(i feel bad) history)
                  '(you seem to think that you feel bad))
    (assert-equal "generate-reply-3"
                  (generate-reply 2 '(i feel bad) history)
                  '(earlier you said that you are sad))
    (assert-equal "generate-reply-4"
                  (generate-reply 3 '(i feel depressed about scheme) history)
                  '(when you feel depressed go out for ice cream))))

;; Test visit-doctor-v2 (interactive, so describe expected behavior)
(define (test-visit-doctor-v2)
  (printf "Test visit-doctor-v2: Run (visit-doctor-v2 'suppertime 2), input (hal), (i feel depressed), (goodbye), then (eric), (i like scheme), (goodbye)\n"))

;; Run all tests
(define (run-tests)
  (printf "=== Running Tests ===\n")
  (test-ask-patient-name)
  (test-has-keywords)
  (test-get-keywords-from-input)
  (test-select-keyword)
  (test-get-groups-for-keyword)
  (test-get-templates-for-keyword)
  (test-pick-random-list)
  (test-replace-star)
  (test-generate-keyword-reply)
  (test-pick-random-vector)
  (test-hedge-answer)
  (test-qualifier-answer)
  (test-many-replace)
  (test-many-replace-v2)
  (test-many-replace-v3)
  (test-change-person)
  (test-update-history)
  (test-history-answer)
  (test-choose-strategy)
  (test-generate-reply)
  (test-visit-doctor-v2)
  (printf "=== All tests completed ===\n")
)

;; Run tests
(run-tests)
