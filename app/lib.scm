(define-macro (define sig . body)
  `(define ,(car sig) (lambda ,(cdr sig) ,@body)))

(define (not x)   (if x #f #t))
(define (null? x) (eqv? x '()))

(define (zero? x) (eqv? x 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))

(define (list . args) args)

(define (first l) (car l))
(define (second l) (car (cdr l)))

(define (caar l) (car (car l)))
(define (cdar l) (cdr (car l)))
(define (cadr l) (car (cdr l)))
(define (cddr l) (cdr (cdr l)))

(define (cadar l) (car (cdr (car l))))

(define (map proc l)
  (if (null? l)
      '()
      (cons (proc (car l))
            (map proc (cdr l)))))

(define-macro (let bindings . expr)
  `(apply (lambda ,(map first bindings) (begin ,@expr))
          (list ,@(map second bindings))))

(define-macro (cond . cond-clauses)
  (define (loop clauses)
    (if (null? clauses)
        '(#t ())
        `(if ,(caar clauses)
             ,(cadar clauses)
             ,(loop (cdr clauses)))))
  (loop cond-clauses))


(define *gensym-counter* 0)

(define (gensym)
  "shitty gensym"
  (set! *gensym-counter* (+ *gensym-counter* 1))
  (string->symbol (string-append "**g"
                                 (number->string *gensym-counter*)
                                 "**")))

