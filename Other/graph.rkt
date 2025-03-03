#lang racket

(require graph)
(require rackunit)

(define world-model (directed-graph
                     '(
                       (Bob Anna) (Anna Bob)
                       (Bob Sam) (Sam Bob)           
                       (Alice Anna) (Anna Alice)
                       (Alice Sam) (Sam Alice)
                       (Alice Bob) (Bob Alice)
                       (Mike Bob) (Bob Mike)
                       (Mike Ben) (Ben Mike))
                       '(
                         dauther father
                         son father
                         dauther mother
                         son morher
                         husband wife
                         son father
                         son father)))

(define (is-parent? first-person second-person)
  (let ((status (edge-weight world-model first-person second-person)))
    (if (or (equal? status 'father) (equal? status 'mother))
        #t #f))
  )

(define (get-parent first-person)
          (foldr (lambda (v l)
                   (if (is-child? v first-person) 
                       (cons v l)
                       l
                       )) '() (get-neighbors world-model first-person)))

(define (get-child first-person)
          (foldr (lambda (v l)
                   (if (is-child? first-person v) 
                       (cons v l)
                       l
                       )) '() (get-neighbors world-model first-person)))

(define (is-child? first-person second-person)
  (let ((status (edge-weight world-model first-person second-person)))
    (if (or (equal? status 'son) (equal? status 'dauther))
        #t #f))
  )

(define (is-sibling? first-person second-person)
  (let loop ((parent-list (get-parent first-person)))
             (cond
               [(equal? parent-list '()) #f]
               [(member second-person
                             (foldr (lambda (v l)
                                      (if (and (is-child? (car parent-list) v) (not (equal? first-person v)))
                                          (cons v l)
                                          l
                                          )) '() (get-neighbors world-model (car parent-list)))
                             ) #t]
               [else (loop (cdr parent-list))]
               )
    )          
  )

(define (get-sibling first-person)
    (foldr (lambda (v l)
         (append
          (filter (lambda (x) (and (not (member x l)) (not (equal? first-person x))))
                  (get-child v))
          l)
         ) '() (get-parent first-person)))

(define (is-grandparent? first-person second-person)
  (let loop ((parent-list (get-parent first-person)))
             (cond
               [(equal? parent-list '()) #f]
               [(member second-person
                             (foldr (lambda (v l)
                                      (if (and (is-parent? (car parent-list) v) (not (equal? first-person v)))
                                          (cons v l)
                                          l
                                          )) '() (get-neighbors world-model (car parent-list)))
                             ) #t]
               [else (loop (cdr parent-list))]
               )
    )          
  )

(define (get-grandparent first-person)
  (foldr (lambda (v l) 
           (append (get-parent v) l)
           ) '() (get-parent first-person))
  )

(define (is-grandchild? first-person second-person)
  (is-grandparent? second-person first-person)
  )

(define (get-grandchild first-person)
  (foldr (lambda (v l) 
           (append (get-child v) l)
           ) '() (get-child first-person))
  )

(define (is-uncle? first-person second-person)
  (let loop ((parent-list (get-parent second-person)))
    (cond
      [(null? parent-list) #f]
      [(is-sibling? first-person (car parent-list)) #t]
      [else (loop (cdr parent-list))])
    )
  )

(define (is-aunt? first-person second-person)
  (let loop ((parent-list (get-parent second-person)))
    (cond
      [(null? parent-list) #f]
      [(is-sibling? first-person (car parent-list)) #t]
      [else (loop (cdr parent-list))])
    )
  )

(define (is-married? first-person second-person)
  (let ((status1 (edge-weight world-model first-person second-person))
        (status2 (edge-weight world-model second-person first-person)))
    (or (equal? status1 'husband) (equal? status1 'wife)
        (equal? status2 'husband) (equal? status2 'wife)))
  )

(check-equal? (get-grandparent 'Anna) '(Mike) "wrong answer")
(check-equal? (is-grandparent? 'Anna 'Mike) #t "wrong answer")
(check-equal? (is-grandparent? 'Anna 'Bob) #f "wrong answer")

(check-equal? (get-parent 'Anna) '(Bob Alice) "wrong answer")
(check-equal? (is-parent? 'Anna 'Bob) #t  "wrong answer")
(check-equal? (is-parent? 'Anna 'Alice) #t  "wrong answer")
(check-equal? (is-parent? 'Anna 'Mike) #f  "wrong answer")

(check-equal? (get-child 'Anna) '() "wrong answer")
(check-equal? (is-child? 'Anna 'Bob) #f  "wrong answer")
(check-equal? (is-child? 'Anna 'Alice) #f  "wrong answer")
(check-equal? (is-child? 'Anna 'Mike) #f  "wrong answer")

(check-equal? (get-grandchild 'Mike) '(Anna Sam) "wrong answer")
(check-equal? (is-grandchild? 'Mike 'Anna) #t "wrong answer")
(check-equal? (is-grandchild? 'Mike 'Sam) #t "wrong answer")
(check-equal? (is-grandchild? 'Mike 'Bob) #f "wrong answer")

(check-equal? (get-sibling 'Bob) '(Ben) "wrong answer")
(check-equal? (is-sibling? 'Bob 'Ben) #t "wrong answer")
(check-equal? (is-sibling? 'Ben 'Bob) #t "wrong answer")

(check-equal? (is-uncle? 'Mike 'Anna) #f "wrong answer")
(check-equal? (is-uncle? 'Bob 'Anna) #f "wrong answer")
(check-equal? (is-uncle? 'Ben 'Anna) #t "wrong answer")

(check-equal? (is-aunt? 'Alice 'Anna) #f "wrong answer")
(check-equal? (is-aunt? 'Alice 'Sam) #f "wrong answer")

(check-equal? (is-married? 'Bob 'Anna) #f "wrong answer")
(check-equal? (is-married? 'Alice 'Bob) #t "wrong answer")
(check-equal? (is-married? 'Bob 'Mike) #f "wrong answer")
