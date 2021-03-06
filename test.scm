(integer? "asdf")
(real? 1)

(+ 1.2 4/5 5) (* 2 3+1.5i) (symbol? 'foo)
(string=? "foo" "bar")
(string=? "foo" "foo")
(= 1 1)
(= 1 1.0)


``(1 ,"foo" ,(2 ,(/ 1.0 5)))

`("expect 1       :" ,(car '(1 2)))
`("expect 1       :" ,(car '(1 . 2)))
`("expect [2]     :" ,(cdr '(1 2)))
`("expect 2       :" ,(cdr '(1 . 2)))
`("expect [1,2]   :" ,(cons 1 '(2)))
`("expect true    :" ,(eqv? 1 1))
`("expect false   :" ,(eqv? 1 2))
`("expect true    :" ,(equal? '(1 2 3) '(1 2 3)))
`("expect false   :" ,(equal? '(1 2 3) '(2 2 3)))

(define foo 1)

`("expect 2: " ,(+ 1 foo))

(set! foo 2)

`("expect 3: " ,(+ 1 foo))

`("expect 2: " ,((lambda (a b) (+ a b))
                 1 1))

(define (foo bar baz)
  (- bar baz)
  (+ bar baz))

`("expect 2: " ,(foo 1 1))

(define (bar) "baz")
bar
(bar)

(define (foo bar . baz) bar)

(foo 1 2 3 4)

(define (foo bar . baz) baz)

(foo 1 2 3 4)

(define (foo . baz) baz)

(foo 1 2 3 4)

(define (add a b) (+ 1 2))

(write-string "Hello, World!\n")

(define (length list)
  (if (equal? list '())
      0
      (+ 1 (length (cdr list)))))


(define (my-avg numbers)
  (/ (apply + numbers)
     (length numbers)))

(define (handler err)
  (write-string "caught exception: ")
  (write err)
  (write-string " continuing...\n"))

(with-exception-handler handler
  (lambda () (raise (bad-form-error "bad form" '()))))

(write
 (with-exception-handler handler
   (lambda () (+ 1 1))))
(write-string "\n")


(define scope-test 1)
((lambda () (define scope-test (+ scope-test 1))))
(write scope-test)
(write-string "\n")
