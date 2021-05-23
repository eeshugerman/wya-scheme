(define-macro (define sig body)
  `(define ,(car sig) (lambda ,(cdr sig) ,body)))

(define (not x)   (if x #f #t))
(define (null? x) (eqv? x '()))

(define (zero? x) (eqv? x 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))

(define (list . args) args)

(define (first l) (car l))
(define (second l) (car (cdr l)))

(define (map proc l)
  (if (null? l)
      '()
      (cons (proc (car l))
            (map proc (cdr l)))))

(define-macro (let bindings expr)
  `(apply (lambda ,(map first bindings) ,expr)
          ',(map second bindings)))
