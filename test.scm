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
