#lang sicp

; A meta-circular pure LISP program 'plisp'
;
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
    (list
     . (lambda x x))
    (null
     . (lambda (x) (eq x '())))
    (not
     . (lambda (x)
         (cond (x '()) (t 't))))
    (and
     . (lambda (x y)
         (cond (x y) (t '()))))
    (or
     . (lambda (x y)
         (cond (x 't) (t y))))
    (rev-append
     . (lambda (x y)
         (cond ((null x) y)
               (t (rev-append (cdr x) (cons (car x) y))))))
    (append
     . (lambda (x y) (rev-append (rev-append x '()) y)))
    (assoclist
     . (lambda (keys values)
         (cond ((null keys) '())
               ((and (not (atom keys)) (not (atom values)))
                (cons (cons (car keys) (car values))
                      (assoclist (cdr keys) (cdr values))))
               ((not (null keys))
                (list (cons keys values))))))
    ; (error
    ;  . (lambda (err-code s-exp)
    ;      (Return a nil with outputing an error code and a S-expression.)))
    (assocv
     . (lambda (key lst)
         (cond ((null lst) (error '1 key))
               ((eq key (caar lst)) (cdar lst))
               (t (assocv key (cdr lst))))))
    (isSUBR
     . (lambda (x)
         (cond ((eq x 'atom) 't)
               ((eq x 'eq) 't)
               ((eq x 'car) 't)
               ((eq x 'cdr) 't)
               ((eq x 'cons) 't)
               ((eq x 'error) 't)
               (t '()))))
    (evcond
     . (lambda (clauses env)
         (cond ((null clauses) '())
               ((null (eval (caar clauses) env))
                (evcond (cdr clauses) env))
               (t (eval (cadar clauses) env)))))
    (evlist
     . (lambda (members env)
         (cond ((null members) '())
               (t (cons (eval (car members) env)
                        (evlist (cdr members) env))))))
    (apply
     . (lambda (func args env)
         (cond
           ((and (atom func) (not (null func)))
            (cond
              ((eq func 'quote) (car args))
              ((eq func 'atom) (cond ((atom (car args)) 't)
                                     (t '())))
              ((eq func 'eq) (cond
                               ((eq (car args) (cadr args)) 't)
                               (t '())))
              ((eq func 'car) (car (car args)))
              ((eq func 'cdr) (cdr (car args)))
              ((eq func 'cons) (cons (car args) (cadr args)))
              ((eq func 'cond) (evcond args env))
              ((eq func 'error) (error (car args) (cadr args)))
              (t (eval (cons (assocv func env) args) env))))
           ((eq (car func) 'label)
            (eval (cons (caddr func) args)
                  (cons (cons (cadr func) (caddr func)) env)))
           ((eq (car func) 'lambda)
            (eval (caddr func)
                  (append (assoclist (cadr func) args) env)))
           (t (error '2 (cons func args))))))
    (eval
     . (lambda (exp env)
         (cond
           ((eq exp 't) 't)
           ((eq exp '()) '())
           ((atom exp) (assocv exp env))
           ((or (isSUBR (car exp))
                (cond ((not (or (atom (car exp)) (null (car exp))))
                       (eq (caar exp) 'lambda))))
            (apply (car exp) (evlist (cdr exp) env) env))
           (t
            (apply (car exp) (cdr exp) env)))))
    ))

; =====================================
;; An evaluator written in Scheme for plisp

(define (atom? x) (not (pair? x)))

(define (assoclist keys values)
  (cond ((null? keys) '())
        ((and (pair? keys) (pair? values))
         (cons (cons (car keys) (car values))
               (assoclist (cdr keys) (cdr values))))
        ((not (null? keys))
         (list (cons keys values)))))

(define (error err-code s-exp)
  (cond ((eqv? err-code 1) (display "Not found"))
        ((eqv? err-code 2) (display "Invalid form"))
        (else (display "Error")))
  (display ": ")
  (display s-exp)
  (newline)
  '())

(define (assocv key lst)
  (cond ((null? lst) (error '1 key))
        ((eq? key (caar lst)) (cdar lst))
        (else (assocv key (cdr lst)))))

(define (isSUBR? x)
  (cond ((eq? x 'atom) #t)
        ((eq? x 'eq) #t)
        ((eq? x 'car) #t)
        ((eq? x 'cdr) #t)
        ((eq? x 'cons) #t)
        ((eq? x 'error) #t)
        (else #f)))

(define (evcond clauses env)
  (cond ((null? clauses) '())
        ((null? (eval_ (caar clauses) env))
         (evcond (cdr clauses) env))
        (else (eval_ (cadar clauses) env))))

(define (evlist members env)
  (cond ((null? members) '())
        (else (cons (eval_ (car members) env)
                    (evlist (cdr members) env)))))

(define (apply_ func args env)
  (cond
    ((and (atom? func) (not (null? func)))
     (cond
       ((eq? func 'quote) (car args))
       ((eq? func 'atom) (cond ((atom? (car args)) 't)
                               (else '())))
       ((eq? func 'eq) (cond ((eq? (car args) (cadr args)) 't)
                             (else '())))
       ((eq? func 'car) (cond ((null? (car args)) '())
                              (else (car (car args)))))
       ((eq? func 'cdr) (cond ((null? (car args)) '())
                              (else (cdr (car args)))))
       ((eq? func 'cons) (cons (car args) (cadr args)))
       ((eq? func 'cond) (evcond args env))
       ((eq? func 'error) (error (car args) (cadr args)))
       (else (eval_ (cons (assocv func env) args) env))))
    ((eq? (car func) 'label)
     (eval_ (cons (caddr func) args)
            (cons (cons (cadr func) (caddr func)) env)))
    ((eq? (car func) 'lambda)
     (eval_ (caddr func)
            (append (assoclist (cadr func) args) env)))
    (else (error '2 (cons func args)))))

(define (eval_ exp env)
  (cond
    ((eq? exp 't) 't)
    ((eq? exp '()) '())
    ((atom? exp) (assocv exp env))
    ((or (isSUBR? (car exp))
         (and (not (or (atom? (car exp)) (null? (car exp))))
              (eq? (caar exp) 'lambda)))
     (apply_ (car exp) (evlist (cdr exp) env) env))
    (else
     (apply_ (car exp) (cdr exp) env))))

; =====================================
;; Virtual REPL

; The global environment list '*env*'
;
(define *env* '((*env*)))

; Add (x . y) to *env*.
;
(define (<< x y)
  (set-cdr! *env* (cons (cons x y) (cdr *env*)))
  (set-cdr! (car *env*) (cdr *env*))
  x)

; Eval x on the environment *env*.
;
(define (>> x) (eval_ x *env*))

; import a environment.
;
(define (importenv! env)
  (set-cdr! *env* env)
  (set-cdr! (car *env*) env)
  't)

; export the environment "*env*.
;
(define (exportenv!) *env*)

; Reset the environment "*env*".
;
(define (resetenv!)
  (set!
   *env*
   (cons (cons '*env* '()) '()))  ; '((*env*)) won't work.
  't)

; The top level expression
;
(define top-exp '())

; Virtual REPL
;
(define (repl-body exp)
  (cond
    ((pair? exp)
     (cond
       ((eq? 'importenv (car exp))
        (display (importenv! (eval (cadr exp) (interaction-environment)))))
       ((eq? 'exportenv (car exp))
        (display (exportenv!)))
       ((eq? 'resetenv (car exp))
        (display (resetenv!)))
       ((eq? 'def (car exp))
        (display (<< (cadr exp) (eval_ (caddr exp) *env*))))
       (else (display (>> exp)))))
    (else (display (>> exp))))
  (newline))
;
(define (read-top)
  (display ">> ")
  (set! top-exp (read)))
;
(define (repl-loop)
  (repl-body top-exp)
  (repl))
;
(define (repl)
  (read-top)
  (cond
    ((pair? top-exp)
     (cond
       ((not (eq? 'exit (car top-exp)))
        (repl-loop))
       (else (display ""))))
    (else (repl-loop))))

; =====================================
;; plisp2

(define plisp2
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
    (list
     . (lambda x x))
    (null
     . (lambda (x) (eq x '())))
    (not
     . (lambda (x)
         (cond (x '()) (t 't))))
    (and
     . (lambda (x y)
         (cond (x y) (t '()))))
    (or
     . (lambda (x y)
         (cond (x 't) (t y))))
    (rev-append
     . (lambda (x y)
         (cond ((null x) y)
               (t (rev-append (cdr x) (cons (car x) y))))))
    (append
     . (lambda (x y) (rev-append (rev-append x '()) y)))
    (assoclist
     . (lambda (keys values)
         (cond ((or (null keys) (null values)) '())
               ((and (not (atom keys)) (not (atom values)))
                (cons (cons (car keys) (car values))
                      (assoclist (cdr keys) (cdr values))))
               ((not (null keys))
                (list (cons keys values))))))
    ; (error
    ;  . (lambda (err-code s-exp)
    ;      (Return a nil with outputing an error code and a S-expression.)))
    (assocv
     . (lambda (key lst)
         (cond ((null lst) (error '1 key))
               ((eq key (caar lst)) (cdar lst))
               (t (assocv key (cdr lst))))))
    (isSUBR
     . (lambda (x)
         (cond ((eq x 'atom) 't)
               ((eq x 'eq) 't)
               ((eq x 'car) 't)
               ((eq x 'cdr) 't)
               ((eq x 'cons) 't)
               ((eq x 'eval) 't)
               ((eq x 'apply) 't)
               ((eq x 'error) 't)
               (t '()))))
    (evcond
     . (lambda (clauses env)
         (cond ((null clauses) '())
               ((null (eval (caar clauses) env))
                (evcond (cdr clauses) env))
               (t (eval (cadar clauses) env)))))
    (evlist
     . (lambda (members env)
         (cond ((null members) '())
               (t (cons (eval (car members) env)
                        (evlist (cdr members) env))))))
    (apply
     . (lambda (func args env)
         (cond
           ((and (atom func) (not (null func)))
            (cond
              ((eq func 'quote) (car args))
              ((eq func 'atom) (cond ((atom (car args)) 't)
                                     (t '())))
              ((eq func 'eq) (cond
                               ((eq (car args) (cadr args)) 't)
                               (t '())))
              ((eq func 'car) (car (car args)))
              ((eq func 'cdr) (cdr (car args)))
              ((eq func 'cons) (cons (car args) (cadr args)))
              ((eq func 'cond) (evcond args env))
              ((eq func 'eval) (eval (car args) (cadr args)))
              ((eq func 'apply) (apply (car args) (cadr args) env))
              ((eq func 'function) (list 'funarg (car args) env))
              ((eq func 'funarg) (cons func args))
              ((eq func 'error) (error (car args) (cadr args)))
              (t (eval (cons (assocv func env) args) env))))
           ((eq (car func) 'label)
            (eval (cons (caddr func) args)
                  (cons (cons (cadr func) (caddr func)) env)))
           ((eq (car func) 'funarg)
            (apply (cadr func) args (caddr func)))
           ((eq (car func) 'lambda)
            (eval (caddr func)
                  (append (assoclist (cadr func) args) env)))
           (t (error '2 (cons func args))))))
    (eval
     . (lambda (exp env)
         (cond
           ((eq exp 't) 't)
           ((eq exp '()) '())
           ((atom exp) (assocv exp env))
           ((or (isSUBR (car exp))
                (cond ((not (or (atom (car exp)) (null (car exp))))
                       (or (eq (caar exp) 'funarg)
                           (eq (caar exp) 'lambda)))))
            (apply (car exp) (evlist (cdr exp) env) env))
           (t
            (apply (car exp) (cdr exp) env)))))
    (funcall
     . (lambda (f . x) (apply f x)))
    (z-combi  ; Z-combinator
     . (lambda (f)
         (funcall
          (function (lambda (y) (f (function (lambda x (apply (y y) x))))))
          (function (lambda (y) (f (function (lambda x (apply (y y) x)))))))))
    ))

; -------------------------------------
