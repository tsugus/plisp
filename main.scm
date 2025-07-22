;; An evaluator written in Scheme for plisp

(define (atom? x) (not (pair? x)))

(define (atom x) (cond ((atom? x) 't)
                       (else '())))

(define (eq x y) (cond ((eq? x y) 't)
                       (else '())))

(define (pairlis* v e a)
  (cond ((null? v) a)
        ((atom? v) (cons (cons v e) a))
        (else (cons (cons (car v) (car e))
                    (pairlis* (cdr v) (cdr e) a)))))

(define (assoc* x a)
  (cond ((null? a) (error '1 x))
        ((eq? x (caar a)) (car a))
        (else (assoc* x (cdr a)))))

(define (error err-code s-exp)
  (cond ((eqv? err-code 1) (display "Not found"))
        ((eqv? err-code 2) (display "Invalid form"))
        (else (display "Error")))
  (display ": ")
  (display s-exp)
  (newline)
  '())

(define (isSUBR? x)
  (cond ((eq? x 'atom) #t)
        ((eq? x 'eq) #t)
        ((eq? x 'car) #t)
        ((eq? x 'cdr) #t)
        ((eq? x 'cons) #t)
        ((eq? x 'error) #t)
        (else #f)))

(define (eval_ e a)
  (cond
    ((eq? e 't) 't)
    ((eq? e '()) '())
    ((atom? e) (cdr (assoc* e a)))
    ((isSUBR? (car e)) (apply_ (car e) (evlis (cdr e) a) a))
    ((atom? (car e)) (apply_ (car e) (cdr e) a))
    ((eq? (caar e) 'lambda) (apply_ (car e) (evlis (cdr e) a) a))
    (else (apply_ (car e) (cdr e) a))))

(define (apply_ fn args a)
  (cond
    ((atom? fn)
     (cond
       ((eq? fn 'quote) (car args))
       ((eq? fn 'atom) (atom (car args)))
       ((eq? fn 'eq) (eq (car args) (cadr args)))
       ((eq? fn 'car) (car (car args)))
       ((eq? fn 'cdr) (cdr (car args)))
       ((eq? fn 'cons) (cons (car args) (cadr args)))
       ((eq? fn 'cond) (evcon args a))
       ((eq? fn 'error) (error (car args) (cadr args)))
       (else (eval_ (cons (cdr (assoc* fn a)) args) a))))
    ((eq? (car fn) 'label)
     (eval_ (cons (caddr fn) args)
            (cons (cons (cadr fn) (caddr fn)) a)))
    ((eq? (car fn) 'lambda)
     (eval_ (caddr fn) (pairlis* (cadr fn) args a)))
    (else (error '2 (cons fn args)))))

(define (evcon c a)
  (cond ((null? (eval_ (caar c) a)) (evcon (cdr c) a))
        (else (eval_ (cadar c) a))))

(define (evlis m a)
  (cond ((null? m) '())
        (else (cons (eval_ (car m) a) (evlis (cdr m) a)))))

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
(define (>> an_eval x)
  ((eval an_eval (interaction-environment)) x *env*))

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
(define *top-exp* '())

; Virtual REPL
;
(define (repl-body an_eval exp)
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
       (else (display (>> an_eval exp)))))
    (else (display (>> an_eval exp))))
  (newline))
;
(define (repl-loop an_eval)
  (repl-body an_eval *top-exp*)
  (repl an_eval))
;
(define repl
  (lambda args
    (if (null? args) (set! args '(eval_)))
    (set! *top-exp* (read))
    (cond
      ((atom? *top-exp*) (repl-loop (car args)))
      ((eq? 'exit (car *top-exp*)) (display ""))
      (else (repl-loop (car args))))))

; =====================================

(load "plisp.scm") (load "plisp2.scm") (load "slisp.scm")
