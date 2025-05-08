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
    (caddar . (lambda (x) (car (cddar x))))
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
    (imply
     . (lambda (x y)
         (cond (x y) (t 't))))
    (rev-append
     . (lambda (x y)
         (cond ((null x) y)
               (t (rev-append (cdr x) (cons (car x) y))))))
    (reverse
     . (lambda (x) (rev-append x '())))
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
    (assoc
     . (lambda (key lst)
         (cond ((null lst) (error '1 key))
               ((eq key (caar lst)) (cdar lst))
               (t (assoc key (cdr lst))))))
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
              (t (eval (cons (assoc func env) args) env))))
           ((eq (car func) 'label)
            (eval (cons (caddr func) args)
                  (cons (cons (cadr func) (caddr func)) env)))
           ((eq (car func) 'lambda)
            (eval (caddr func)
                  (append (assoclist (cadr func) (evlist args env))
                          env)))
           (t (error '2 (cons func args))))))
    (eval
     . (lambda (form env)
         (cond
           ((eq form 't) 't)
           ((eq form '()) '())
           ((atom form) (assoc form env))
           ((isSUBR (car form))
            (apply (car form) (evlist (cdr form) env) env))
           (t
            (apply (car form) (cdr form) env)))))
    ))

; -------------------------------------
; An evaluator written in Scheme for plisp

(define (atom? x) (not (pair? x)))

(define (assoclist keys values)
  (cond ((or (null? keys) (null? values)) '())
        ((and (not (atom? keys)) (not (atom? values)))
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

(define (assoc_ key lst)
  (cond ((null? lst) (error '1 key))
        ((eq? key (caar lst)) (cdar lst))
        (else (assoc_ key (cdr lst)))))

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
       (else (eval_ (cons (assoc_ func env) args) env))))
    ((eq? (car func) 'label)
     (eval_ (cons (caddr func) args)
            (cons (cons (cadr func) (caddr func)) env)))
    ((eq? (car func) 'lambda)
     (eval_ (caddr func)
            (append (assoclist (cadr func) (evlist args env))
                    env)))
    (eval (error '2 (cons func args)))))

(define (eval_ form env)
  (cond
    ((eq? form 't) 't)
    ((eq? form '()) '())
    ((atom? form) (assoc_ form env))
    ((isSUBR? (car form))
     (apply_ (car form) (evlist (cdr form) env) env))
    (else
     (apply_ (car form) (cdr form) env))))

; -------------------------------------
; Complete self-embedding

(define plisp-pair '(plisp))
(set! plisp (cons plisp-pair plisp))
(set-cdr! plisp-pair plisp)

; -------------------------------------
; "Pseudo interpretor" functions

; The global environment list '*env*'
;
(define *env* plisp)

; Add (x . y) to *env*.
;
(define (<< x y)
  (let ()
    (set! *env* (cons (cons x y) *env*))
    (set-cdr! plisp-pair *env*)
    x))

; Eval x on *env*.
;
(define (>> x) (eval_ x *env*))

; Reset *env.
;
(define (reset!)
  (let ()
    (set! *env* plisp)
    (set-cdr! plisp-pair plisp)
    't))

; =====================================
;; Test 1
'Test_1

(<< 'reverse2
    '(lambda (x)
       ((label
         rec
         (lambda (x y)
           (cond ((null x) y)
                 (t (rec (cdr x) (cons (car x) y))))))
        x '())))
(>> '(eval '(eval '(reverse2 '(a b c d)) plisp) plisp))
(>> '(eval '(eval 'reverse2 plisp) plisp))
(reset!)
(>> '(eval '(eval 'reverse2 plisp) plisp))
(>> '(eval '(eval '(cddr '(a b c d)) plisp) plisp))

; -------------------------------------
;; Redefine

(<< 'isSUBR
    '(lambda (x)
       (cond ((eq x 'atom) 't)
             ((eq x 'eq) 't)
             ((eq x 'car) 't)
             ((eq x 'cdr) 't)
             ((eq x 'cons) 't)
             ((eq x 'eval) 't)
             ((eq x 'apply) 't)
             ((eq x 'error) 't)
             (t '()))))

(<< 'apply
    '(lambda (func args env)
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
            (t (eval (cons (assoc func env) args) env))))
         ((eq (car func) 'label)
          (eval (cons (caddr func) args)
                (cons (cons (cadr func) (caddr func)) env)))
         ((eq (car func) 'funarg)
          (apply (cadr func) args (caddr func)))
         ((eq (car func) 'lambda)
          (eval (caddr func)
                (append (assoclist (cadr func) args) env)))
         (t (error '2 (cons func args))))))

(<< 'eval
    '(lambda (form env)
       (cond
         ((eq form 't) 't)
         ((eq form '()) '())
         ((atom form) (assoc form env))
         ((or (isSUBR (car form))
              (cond ((not (or (atom (car form)) (null (car form))))
                     (or (eq (caar form) 'funarg)
                         (eq (caar form) 'lambda)))))
          (apply (car form) (evlist (cdr form) env) env))
         (t
          (apply (car form) (cdr form) env)))))

; -------------------------------------
; Z-combinator

(<< 'funcall
    '(lambda (f . x) (apply f x)))

(<< 'z-combi
    '(lambda (f)
       (funcall
        (function (lambda (y) (f (function (lambda x (apply (y y) x))))))
        (function (lambda (y) (f (function (lambda x (apply (y y) x)))))))))

; -------------------------------------
;; Test 2
'Test_2

(<< 'reverse_z
    '(lambda (l)
       (funcall
        (z-combi
         (function (lambda (f)
                     (function (lambda (l acc)
                                 (cond
                                   ((eq '() l) acc)
                                   (t (f (cdr l)
                                         (cons (car l) acc)))))))))
        l '())))
(>> '(eval '(reverse_z '(1 2 3 4 5 6 7 8 9 10)) plisp))
