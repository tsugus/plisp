(display "Test of 'eval_'")
(newline)

(display
 (eval_
  '((lambda (x)
      ((label
        rev
        (lambda (x y)
          (cond ((eq '() x) y)
                (t (rev (cdr x) (cons (car x) y))))))
       x '())) '(1 2 3 4 5))
  '()))
(newline)
(display
 (eval_
  '((label
     subst
     (lambda (x y z)
       (cond ((atom z) (cond ((eq z y) x)
                             ('t z)))
             ('t (cons (subst x y (car z))
                       (subst x y (cdr z)))))))
    'm 'b '(a b (a b c) d))
  '()))
(newline)

(newline)
(display "Test of 'eval_s'")
(newline)

(display
 (eval_s
  '((lambda (x)
      ((label
        rev
        (lambda (x y)
          (cond ((eq '() x) y)
                (t (rev (cdr x) (cons (car x) y))))))
       x '())) '(1 2 3 4 5))
  '()))
(newline)
(display
 (eval_s
  '((label
     subst
     (lambda (x y z)
       (cond ((atom z) (cond ((eq z y) x)
                             (t z)))
             (t (cons (subst x y (car z))
                      (subst x y (cdr z)))))))
    'm 'b '(a b (a b c) d))
  '()))
(newline)

(newline)
(display "Test of Z-combinator & Continuation-Passing Style")
(newline)

(define reverse_z
  '(lambda (x)
     (funcall
      (z-combi
       (function
        (lambda (f)
          (function
           (lambda (x y)
             (cond
               ((null x)
                y)
               (t
                (f (cdr x)
                   (cons (car x) y)))))))))
      x '())))

(define append_z
  '(lambda (l m)
     ((lambda (cont)
        (funcall
         (z-combi
          (function
           (lambda (f)
             (function
              (lambda (l m cont)
                (cond
                  ((null l)
                   (cont m))
                  (t (f (cdr l)
                        m
                        (function
                         (lambda (x)
                           (cont (cons (car l) x))))))))))))
         l m cont))
      (function (lambda (x) x)))))

(define env plisp2)
(set! env (cons (cons 'reverse reverse_z) env))
(set! env (cons (cons 'append append_z) env))
(set! env (cons (cons 'env env) env))

(display
 (eval_ '(eval '(reverse '(1 2 3 4 5)) env) env))
(newline)
(display
 (eval_ '(eval '(append '(a b c) '(d e)) env) env))
