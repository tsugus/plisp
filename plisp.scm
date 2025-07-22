;; A meta-circular pure LISP program 'plisp'

(define plisp
  '(
    (caar . (lambda (x) (car (car x))))
    (cadr . (lambda (x) (car (cdr x))))
    (cdar . (lambda (x) (cdr (car x))))
    (cddr . (lambda (x) (cdr (cdr x))))
    (caaar . (lambda (x) (car (caar x))))
    (caadr . (lambda (x) (car (cadr x))))
    (cadar . (lambda (x) (car (cdar x))))
    (caddr . (lambda (x) (car (cddr x))))
    (cdaar . (lambda (x) (cdr (caar x))))
    (cdadr . (lambda (x) (cdr (cadr x))))
    (cddar . (lambda (x) (cdr (cdar x))))
    (cdddr . (lambda (x) (cdr (cddr x))))
    (null
     . (lambda (x) (eq x '())))
    (not
     . (lambda (x)
         (cond (x '()) (t 't))))
    (pairlis
     . (lambda (v e a)
         (cond ((null v) a)
               ((atom v) (cons (cons v e) a))
               (t (cons (cons (car v) (car e))
                        (pairlis (cdr v) (cdr e) a))))))
    (assoc
     . (lambda (x a)
         (cond ((null a) (error '1 x))
               ((eq x (caar a)) (car a))
               (t (assoc x (cdr a))))))
    ; (error
    ;  . (lambda (err-code s-exp)
    ;      (Return a nil with outputing an error code and a S-expression.)))
    (isSUBR
     . (lambda (x)
         (cond ((eq x 'atom) 't)
               ((eq x 'eq) 't)
               ((eq x 'car) 't)
               ((eq x 'cdr) 't)
               ((eq x 'cons) 't)
               ((eq x 'error) 't)
               (t '()))))
    (eval
     . (lambda (e a)
         (cond
           ((eq e 't) 't)
           ((eq e '()) '())
           ((atom e) (cdr (assoc e a)))
           ((isSUBR (car e)) (apply (car e) (evlis (cdr e) a) a))
           ((atom (car e)) (apply (car e) (cdr e) a))
           ((eq (caar e) 'lambda) (apply (car e) (evlis (cdr e) a) a))
           (t (apply (car e) (cdr e) a)))))
    (apply
     . (lambda (fn args a)
         (cond
           ((atom fn)
            (cond
              ((eq fn 'quote) (car args))
              ((eq fn 'atom) (atom (car args)))
              ((eq fn 'eq) (eq (car args) (cadr args)))
              ((eq fn 'car) (car (car args)))
              ((eq fn 'cdr) (cdr (car args)))
              ((eq fn 'cons) (cons (car args) (cadr args)))
              ((eq fn 'cond) (evcon args a))
              ((eq fn 'error) (error (car args) (cadr args)))
              (t (eval (cons (cdr (assoc fn a)) args) a))))
           ((eq (car fn) 'label)
            (eval (cons (caddr fn) args)
                  (cons (cons (cadr fn) (caddr fn)) a)))
           ((eq (car fn) 'lambda)
            (eval (caddr fn) (pairlis (cadr fn) args a)))
           (t (error '2 (cons fn args))))))
    (evcon
     . (lambda (c a)
         (cond ((null (eval (caar c) a)) (evcon (cdr c) a))
               (t (eval (cadar c) a)))))
    (evlis
     . (lambda (m a)
         (cond ((null m) '())
               (t (cons (eval (car m) a) (evlis (cdr m) a))))))
    ))
