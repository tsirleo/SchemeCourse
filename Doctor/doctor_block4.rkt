#lang scheme/base

(require (rename-in racket/vector [vector-append racket-vector-append]))
(require racket/list)

(define (my-vector-member elem vec)
  (let loop ((i 0))
    (if (>= i (vector-length vec))
        #f
        (if (equal? elem (vector-ref vec i))
            i
            (loop (+ i 1))))))

(define (my-vector-append v1 v2)
  (racket-vector-append v1 v2))

(define (vector-take v n)
  (vector-copy v 0 n))

(define (vector-empty? v) (= (vector-length v) 0))

(define mock-random-values '())
(define (mock-random low high)
  (if (null? mock-random-values)
      (error "No more mock random values")
      (let ((value (car mock-random-values)))
        (set! mock-random-values (cdr mock-random-values))
        value)))

(define (random low high)
  (mock-random low high))

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
                (doctor-driver-loop-v2 name (vector) (vector))
                (loop (+ patients-served 1))))))))

(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop-v2 name (vector) (vector)))

(define (doctor-driver-loop-v2 name history keyword-history)
  (newline)
  (print '**)
  (let ((user-response (read)))
    (cond 
      ((equal? user-response '(goodbye))
       (printf "Goodbye, ~a!\n" name)
       (print '(see you next week)))
      (else 
       (let* ((new-history (update-history history user-response))
              (new-keyword-history (update-keyword-history keyword-history user-response))
              (reply-text (reply-v2 strategies control-strategies user-response new-history new-keyword-history)))
         (print reply-text)
         (doctor-driver-loop-v2 name new-history new-keyword-history))))))

(define (update-history history new-response)
  (if (my-vector-member new-response history)
      history
      (let ((new-history (my-vector-append (vector new-response) history)))
        (if (> (vector-length new-history) 7)
            (vector-take new-history 7)
            new-history))))

(define (update-keyword-history keyword-history user-response)
  (let ((keywords (get-keywords-from-input user-response)))
    (if (null? keywords)
        keyword-history
        (let ((new-entry (cons (car keywords) (get-group-index (car keywords)))))
          (let ((new-history (my-vector-append (vector new-entry) keyword-history)))
            (if (> (vector-length new-history) 5)
                (vector-take new-history 5)
                new-history))))))

(define (has-keywords? user-response)
  (ormap (lambda (word) (not (not (member word all-keywords)))) user-response))

(define (get-keywords-from-input input)
  (filter (lambda (word) (member word all-keywords)) input))

(define (get-group-index keyword)
  (let loop ((i 0))
    (if (>= i (vector-length keywords-structure))
        #f
        (if (member keyword (vector->list (vector-ref (vector-ref keywords-structure i) 0)))
            i
            (loop (+ i 1))))))

(define (select-keyword input keyword-history)
  (let ((keywords-in-input (get-keywords-from-input input)))
    (if (null? keywords-in-input)
        #f
        (let* ((last-group (if (vector-empty? keyword-history)
                              #f
                              (cdr (vector-ref keyword-history 0))))
               (available-keywords
                (if (and last-group (ormap (lambda (kw) (member kw (vector->list (vector-ref (vector-ref keywords-structure last-group) 0)))) keywords-in-input))
                    (filter (lambda (kw) (member kw (vector->list (vector-ref (vector-ref keywords-structure last-group) 0)))) keywords-in-input)
                    keywords-in-input))
               (used-keywords (map car (vector->list keyword-history))))
          (let ((filtered-keywords (filter (lambda (kw) (not (member kw used-keywords))) available-keywords)))
            (if (null? filtered-keywords)
                (list-ref available-keywords (random 0 (length available-keywords)))
                (list-ref filtered-keywords (random 0 (length filtered-keywords)))))))))

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

(define (generate-keyword-reply user-response history keyword-history)
  (let* ((keyword (select-keyword user-response keyword-history))
         (templates (if keyword (get-templates-for-keyword keyword) (vector->list (vector-ref (vector-ref keywords-structure 0) 1))))
         (template (pick-random-list templates))
         (reply (if keyword (replace-star template keyword) template)))
    reply))

(define (hedge-answer user-response history)
  (pick-random-vector '#((please go on)
                         (many people have the same sorts of feelings)
                         (many of my patients have told me the same thing)
                         (please continue)
                         (tell me more)
                         (i see)
                         (interesting))))

(define (qualifier-answer user-response history)
  (append (pick-random-vector '#((you seem to think that)
                                 (you feel that)
                                 (why do you believe that)
                                 (why do you say that)
                                 (what makes you think that)
                                 (do you really believe that)
                                 (is that so)))
          (change-person user-response)))

(define (history-answer user-response history)
  (let* ((random-index (random 0 (vector-length history)))
         (selected-response (vector-ref history random-index))
         (changed-response (change-person selected-response)))
    (append '(earlier you said that) changed-response)))

(define (pick-random-vector vctr)
  (vector-ref vctr (random 0 (vector-length vctr))))

(define (change-person phrase)
  (many-replace '((am are)
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
  (map (lambda (elem)
         (let ((pat-rep (assoc elem replacement-pairs)))
           (if pat-rep (cadr pat-rep) elem)))
       lst))

(define (vector-foldl f init vctr)
  (let ((length (vector-length vctr)))
    (let loop ((i 0) (result init))
      (if (= i length) result
          (loop (add1 i) (f i result (vector-ref vctr i)))))))

(define (make-strategy checker weight body)
  (vector checker weight body))

(define (strategy-checker strategy)
  (vector-ref strategy 0))

(define (strategy-weight strategy)
  (vector-ref strategy 1))

(define (strategy-body strategy)
  (vector-ref strategy 2))

(define (filter-applicable-strategies strategies user-response history)
  (list->vector
   (filter (lambda (strategy)
             ((strategy-checker strategy) user-response history))
           (vector->list strategies))))

(define (pick-random-vector-with-weight vec get-weight)
  (if (vector-empty? vec)
      (error "No applicable strategies")
      (let* ((total-weight (vector-foldl (lambda (i sum elem) (+ sum (get-weight elem))) 0 vec))
             (rand (random 0 total-weight)))
        (call/cc
         (lambda (return)
           (vector-foldl
            (lambda (i cumulative elem)
              (let ((new-cumulative (+ cumulative (get-weight elem))))
                (if (< rand new-cumulative)
                    (return elem)
                    new-cumulative)))
            0
            vec)
           (error "Should not reach here"))))))

(define (always-true user-response history) #t)
(define (history-not-empty user-response history) (not (vector-empty? history)))
(define (has-keywords-checker user-response history) (has-keywords? user-response))

(define strategies
  (vector
   (make-strategy always-true 1 hedge-answer)
   (make-strategy always-true 2 qualifier-answer)
   (make-strategy history-not-empty 3 history-answer)
   (make-strategy has-keywords-checker 4 generate-keyword-reply)))

(define (make-control-strategy checker weight body)
  (vector checker weight body))

(define (control-strategy-checker c-strategy)
  (vector-ref c-strategy 0))

(define (control-strategy-weight c-strategy)
  (vector-ref c-strategy 1))

(define (control-strategy-body c-strategy)
  (vector-ref c-strategy 2))

(define (weighted-random-strategy user-response history strategies)
  (let ((applicable (filter-applicable-strategies strategies user-response history)))
    (if (vector-empty? applicable)
        (error "No applicable strategies")
        (pick-random-vector-with-weight applicable strategy-weight))))

(define (cond-strategy user-response history strategies)
  (let ((sorted-strategies
         (sort (vector->list strategies)
               (lambda (s1 s2) (>= (strategy-weight s1) (strategy-weight s2))))))
    (let loop ((remaining sorted-strategies))
      (if (null? remaining)
          (error "No applicable strategies")
          (let ((strategy (car remaining)))
            (if ((strategy-checker strategy) user-response history)
                strategy
                (loop (cdr remaining))))))))

(define (recent-keyword-heavy user-response history)
  (and (has-keywords? user-response)
       (not (vector-empty? history))
       (has-keywords? (vector-ref history 0))))

(define (varied-response-needed user-response history)
  (and (not (vector-empty? history))
       (>= (vector-length history) 3)
       (let ((last-three (vector-take history 3)))
         (let ((strategies-used
                (map (lambda (resp) (has-keywords? resp)) (vector->list last-three))))
           (andmap (lambda (x) x) strategies-used)))))

(define control-strategies
  (vector
   (make-control-strategy recent-keyword-heavy 3 weighted-random-strategy)
   (make-control-strategy varied-response-needed 2 cond-strategy)))

(define (reply-v2 strategies control-strategies user-response history keyword-history)
  (let* ((applicable-controls (filter-applicable-strategies control-strategies user-response history))
         (selected-control (if (vector-empty? applicable-controls)
                              (vector-ref control-strategies 0) ; Default to weighted-random
                              (pick-random-vector-with-weight applicable-controls control-strategy-weight)))
         (selected-strategy ((control-strategy-body selected-control) user-response history strategies)))
    ((strategy-body selected-strategy) user-response history keyword-history)))

(define (assert-equal test-name actual expected)
  (if (equal? actual expected)
      (printf "Test ~a: PASSED\n" test-name)
      (printf "Test ~a: FAILED (expected ~a, got ~a)\n" test-name expected actual)))

(define (test-update-keyword-history)
  (let ((kh1 (vector))
        (kh2 (vector (cons 'depressed 0))))
    (assert-equal "update-keyword-history-1"
                  (update-keyword-history kh1 '(i feel depressed))
                  (vector (cons 'depressed 0)))
    (assert-equal "update-keyword-history-2"
                  (update-keyword-history kh2 '(i love my mother))
                  (vector (cons 'mother 1) (cons 'depressed 0)))))

(define (test-select-keyword)
  (set! mock-random-values '(0 1))
  (let ((kh (vector (cons 'mother 1))))
    (assert-equal "select-keyword-1"
                  (select-keyword '(i love my father brother) kh)
                  'father)
    (assert-equal "select-keyword-2"
                  (select-keyword '(i love my father brother) kh)
                  'brother)))

(define (test-recent-keyword-heavy)
  (let ((history (vector '(i feel depressed))))
    (assert-equal "recent-keyword-heavy-1"
                  (recent-keyword-heavy '(i love my mother) history)
                  #t)
    (assert-equal "recent-keyword-heavy-2"
                  (recent-keyword-heavy '(i like to eat) history)
                  #f)))

(define (test-varied-response-needed)
  (let ((history (vector '(i feel depressed) '(i love my mother) '(i study at university))))
    (assert-equal "varied-response-needed-1"
                  (varied-response-needed '(i feel sad) history)
                  #t)
    (assert-equal "varied-response-needed-2"
                  (varied-response-needed '(i like to eat) history)
                  #t)))

(define (run-tests)
  (printf "=== Running Tests ===\n")
  (test-update-keyword-history)
  (test-select-keyword)
  (test-recent-keyword-heavy)
  (test-varied-response-needed)
  (printf "=== All tests completed ===\n")
)

(run-tests)
