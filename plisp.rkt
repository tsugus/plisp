#lang sicp

; The meta-circular pure LISP program 'plisp'
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
     . (lambda (x) (cond (x '())
                         (t 't))))
    (and
     . (lambda (x y)
         (cond ((null x) '())
               (t (cond (y 't)
                        (t '()))))))
    (or
     . (lambda (x y)
         (cond (x 't)
               (t (cond ((null y) '())
                        (t 't))))))
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
    (assoc
     . (lambda (key lst)
         (cond ((null lst) '())
               ((eq key (caar lst)) (cdar lst))
               (t (assoc key (cdr lst))))))
    (isSUBR
     . (lambda (x)
         (cond ((eq x 'atom) 't)
               ((eq x 'eq) 't)
               ((eq x 'car) 't)
               ((eq x 'cdr) 't)
               ((eq x 'cons) 't)
               ((eq x 'eval) 't)
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
              (t (eval (cons (assoc func env) args) env))))
           ((eq (car func) 'label)
            (eval (cons (caddr func) args)
                  (cons (cons (cadr func) (caddr func)) env)))
           ((eq (car func) 'lambda)
            (eval (caddr func)
                  (append (assoclist (cadr func) (evlist args env))
                          env))))))
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

(define (assoc_ key lst)
  (cond ((null? lst) '())
        ((eq? key (caar lst)) (cdar lst))
        (else (assoc_ key (cdr lst)))))

(define (isSUBR? x)
  (cond ((eq? x 'atom) #t)
        ((eq? x 'eq) #t)
        ((eq? x 'car) #t)
        ((eq? x 'cdr) #t)
        ((eq? x 'cons) #t)
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
       ((eq? func 'eq) (cond
                         ((eq? (car args) (cadr args)) 't)
                         (else '())))
       ((eq? func 'car) (car (car args)))
       ((eq? func 'cdr) (cdr (car args)))
       ((eq? func 'cons) (cons (car args) (cadr args)))
       ((eq? func 'cond) (evcond args env))
       (else (eval_ (cons (assoc_ func env) args) env))))
    ((eq? (car func) 'label)
     (eval_ (cons (caddr func) args)
            (cons (cons (cadr func) (caddr func)) env)))
    ((eq? (car func) 'lambda)
     (eval_ (caddr func)
            (append (assoclist (cadr func) (evlist args env))
                    env)))))

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

; ---------------------------------------

; Ex. 1
;
(<< 'reverse2
    '(lambda (x)
       ((label
         rec
         (lambda (x y)
           (cond ((null x) y)
                 (t (rec (cdr x) (cons (car x) y))))))
        x '())))

; Ex. 2
;
(>> '(eval '(eval '(eval '(reverse2 '(a b c d)) plisp) plisp) plisp))

; Ex. 3
;
(>> '(eval '(eval '(eval 'reverse2 plisp) plisp) plisp))
(reset!)
(>> '(eval '(eval '(eval 'reverse2 plisp) plisp) plisp))
(>> '(eval '(eval '(eval '(cddr '(a b c d)) plisp) plisp) plisp))
