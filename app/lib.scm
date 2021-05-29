(define-macro (define sig . body)
  `(define ,(car sig) (lambda ,(cdr sig) ,@body)))

(define (not x)   (if x #f #t))
(define (null? x) (eqv? x '()))

(define (zero? x) (eqv? x 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))

(define (list . args) args)

(define (length list)
  (if (equal? list '())
      0
      (+ 1 (length (cdr list)))))

(define (first l) (car l))
(define (second l) (car (cdr l)))
(define (third l) (car (cdr (cdr l))))

(define (caar l) (car (car l)))
(define (cdar l) (cdr (car l)))
(define (cadr l) (car (cdr l)))
(define (cddr l) (cdr (cdr l)))

(define (map proc l)
  (if (null? l)
      '()
      (cons (proc (car l))
            (map proc (cdr l)))))

(define-macro (let bindings . expr)
  `(apply (lambda ,(map first bindings) (begin ,@expr))
          (list ,@(map second bindings))))

(define-macro (cond . all-clauses)
  (define (loop clauses)
    (if (null? clauses)
        ''()
        (let ((clause (car clauses))
              (clause-length (length (car clauses))))
          (if (= clause-length 1)
              `(or ,clause
                   ,(loop (cdr clauses)))
              (if (= clause-length 2)
                  (if (equal? (car clause) 'else)
                      (if (= (length clauses) 1)
                          `(begin ,@(cdr clause))
                          (raise (bad-form-error
                                  "`else' in invalid position"
                                  (cons 'cond all-clauses))))
                      `(if ,(car clause)
                           (begin ,@(cdr clause))
                           ,(loop (cdr clauses))))
                  (if (and (= clause-length 3)
                           (equal? (second clause) '=>))
                      (let ((test-result-var (gensym)))
                        `(let ((,test-result-var ,(car clause)))
                           (if ,test-result-var
                               (,(third clause) ,test-result-var)
                               ,(loop (cdr clauses)))))
                      (raise (bad-form-error
                              "invalid `cond' clause'"
                              clause))))))))
  (loop all-clauses))


(define *gensym-counter* 0)

(define (gensym)
  "shitty gensym"
  (set! *gensym-counter* (+ *gensym-counter* 1))
  (string->symbol (string-append "**g"
                                 (number->string *gensym-counter*)
                                 "**")))

