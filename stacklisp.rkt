#lang sicp

;; Basic Functions

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

; =====================================
;;; Stack and GOTO System
;;   (GOTO is freedom!
;;   Don't be afraid of spaghetti programs,
;;   because GOTO [or jump] makes Tail Recursion Optimization easy.)
;
; An Example of "Flowchart List"
;
; ((label START)
;  <S-exp>
;  ...
;  <S-exp>
;  (branch 'Name1 'Name2)
;  (label Name1)
;  <S-exp>
;  ...
;  <S-exp>
;  (return)
;  (label Name2)
;  <S-exp>
;  ...
;  <S-exp>
;  (goto 'START))
;
; Note:
;  Each element is a S-exprresion.
;  The "flow" cannot jump over labels.
;  The first label must be "START".
;  "Flowchart list" can be converted to "program list".
;  If you give a name to a "program list" with "define",
; you can call a subroutine with "call".
;  "call" must specify a return label.
;  "goto" is only valid within the same subroutine;
; to go to another subroutine, use "jump."
;
; An Example of "Program"
;
; ((START (<S-exp> ... <S-exp> (branch 'Name1 'Name2)))
;  (Name1 (<S-exp> ... <S-exp> (return)))
;  (Name2 (<S-exp> ... <S-exp> (goto 'START))))
;

; convert a "flowchart list" to a "program list"
(define (prog<-flow exp)
  (prog<-wolf '() '() (reverse exp)))

(define (prog<-wolf lst terms exp)
  (cond ((atom? exp) lst)
        ((and (pair? (car exp)) (eq? 'label (caar exp)))
         (prog<-wolf (cons (cons (cadar exp) terms) lst) '() (cdr exp)))
        (else (prog<-wolf lst (cons (car exp) terms) (cdr exp)))))

; program counter "(label . function)"
(define PC '(()))

; data stack
(define dstack '())

; return stack
(define rstack '())

; push to the data stack
(define (dpush v)
  (set! dstack (cons v dstack)))

; push to the return stack
(define (rpush v)
  (set! rstack (cons v rstack)))

; pop from the data stack
(define (dpop)
  (let ((t (car dstack))) (set! dstack (cdr dstack)) t))

; pop from the return stack
(define (rpop)
  (let ((t (car rstack))) (set! rstack (cdr rstack)) t))

; goto
(define (goto label)
  (set! PC (cons label (cdr PC))))

; branch
(define (branch yes no)
  (goto (if (dpop) yes no)))

; return
(define (return)
  (set! PC (rpop)))

; call
(define (call func return_label)
  (rpush (cons return_label (cdr PC)))
  (set! PC (cons 'START func)))

; jump
(define (jump func)
  (set! PC (cons 'START func))
  (run))

(define (my_eval exp) (eval exp (interaction-environment)))

(define (exec terms)
  (cond ((atom? terms) 'ok)
        (else
         (my_eval (car terms))
         (exec (cdr terms)))))

(define (run)
  (cond ((eq? '() (car PC)) 'ok)
        (else
         (exec (assocv (car PC)
                       (my_eval (cdr PC))))
         (run))))

(define (do func)
  (call func '()) (run) (dpop))

;; Stack Operations

(define (dup)
  (set! dstack (cons (car dstack) dstack)))

(define (over)
  (set! dstack (cons (cadr dstack) dstack)))

(define (swap)
  (let ((t (dpop)) (s (dpop))) (dpush t) (dpush s)))

(define (rot)
  (let ((top (dpop)) (sec (dpop)) (thir (dpop)))
    (dpush sec) (dpush top) (dpush thir)))

(define (.S) (display dstack) (newline))

; clear stacks and program counter
(define (clearsys)
  (set! dstack '())
  (set! rstack '())
  (set! PC '(())))

;; Stack Version "eval"

(define (eval_ exp env)
  (clearsys)
  (dpush env)
  (dpush exp)
  (do 'eval__))

; =====================================
;; Pure LISP Interpreter for Stack and GOTO System

(define eval__
  (prog<-flow
   '((label START)    ; dstack = [exp env]
     (dup)                     ; [exp exp env]
     (dpush (eq? (dpop) 't))   ; [(eq? exp 't) exp env]
     (branch 'T 'IS_NIL?)      ; [exp env]

     (label T)                 ; [exp env]
     (dpop)                    ; [env]
     (dpop)                    ; []
     (dpush 't)                ; [t]
     (return)

     (label IS_NIL?)           ; [exp env]
     (dup)                     ; [exp exp env]
     (dpush (eq? (dpop) '()))  ; [(eq? exp '()) exp env]
     (branch 'NIL 'IS_ATOM?)   ; [exp env]

     (label NIL)               ; [exp env]
     (dpop)                    ; [env]
     (dpop)                    ; []
     (dpush '())               ; [()]
     (return)

     (label IS_ATOM?)          ; [exp env]
     (dup)                     ; [exp exp env]
     (dpush (atom? (dpop)))    ; [(atom? exp) exp env]
     (branch 'ATOM 'IS_SUBR?)  ; [exp env]

     (label ATOM)              ; [exp env]
     (dpush
      (assocv (dpop) (dpop)))  ; [(assocv exp env)]
     (return)

     (label IS_SUBR?)          ; [exp env]
     (dup)                     ; [exp exp env]
     (dpush
      (isSUBR? (car (dpop))))  ; [(isSUBR? (car exp)) exp env]
     (branch 'SUBR 'OTHERS)    ; [exp env]

     (label SUBR)              ; [exp env]
     (over)                    ; [env exp env]
     (over)                    ; [exp env exp env]
     (dpush (cdr (dpop)))      ; [(cdr exp) env exp env]
     (call 'evlist__ 'SUBR_2)  ; [(evlist (cdr exp) env) exp env]

     (label SUBR_2)            ; [(evlist (cdr exp) env) exp env]
     (swap)                    ; [exp (evlist (cdr exp) env) env]
     (dpush (car (dpop)))      ; [(car exp) (evlist (cdr exp) env) env]
     (jump 'apply__)           ; [(apply (car exp) (evlist (cdr exp) env) env)]

     (label OTHERS)            ; [exp env]
     (dup)                     ; [exp exp env]
     (dpush (cdr (dpop)))      ; [(cdr exp) exp env]
     (swap)                    ; [exp (cdr exp) env]
     (dpush (car (dpop)))      ; [(car exp) (cdr exp) env]
     (jump 'apply__)           ; [apply (car exp) (cdr exp) env]
     )))


(define apply__
  (prog<-flow
   '((label START)    ; dstack = [func args env]
     (dup)                     ; [func func args env]
     (dup)                     ; [func func func args env]
     (dpush
      (let ((x (dpop))
            (y (dpop)))        ; Support a special form "and".
        (and
         (atom? x)
         (not (null? y)))))    ; [(and (atom? func) (not (null? func))) func args env]
     (branch 'IS_QUOTE?
             'IS_LABEL?)       ; [func args env]

     (label IS_QUOTE?)         ; [func args env]
     (dup)                     ; [func func args env]
     (dpush
      (eq? (dpop) 'quote))     ; [(eq? func 'quote) func args env]
     (branch 'QUOTE
             'IS_ATOM?)        ; [func args env]

     (label QUOTE)             ; [func args env]
     (dpop)                    ; [args env]
     (dpush (car (dpop)))      ; [(car args) env]
     (swap)                    ; [env (car args)]
     (dpop)                    ; [(car args)]
     (return)

     (label IS_ATOM?)          ; [func args env]
     (dup)                     ; [func func args env]
     (dpush
      (eq? (dpop) 'atom))      ; [(eq? func 'atom) func args env]
     (branch 'ATOM 'IS_EQ?)    ; [func args env]

     (label ATOM)              ; [func args env]
     (dpop)                    ; [args env]
     (dpush
      (cond
        ((atom? (car (dpop)))
         't)
        (else
         '())))                ; [(cond ((atom? (car args)) 't) (else '())) env]
     (swap)                    ; [env (cond ((atom? (car args)) 't) (else '()))]
     (dpop)                    ; [(cond ((atom? (car args)) 't) (else '()))]
     (return)

     (label IS_EQ?)            ; [func args env]
     (dup)                     ; [func func args env]
     (dpush (eq? (dpop) 'eq))  ; [(eq? func 'eq) func args env]
     (branch 'EQ 'IS_CAR?)     ; [func args env]

     (label EQ)                ; [func args env]
     (dpop)                    ; [args env]
     (dup)                     ; [args args env]
     (dpush
      (cond
        ((eq? (car (dpop))
              (cadr (dpop)))
         't)
        (else
         '())))                ; [(cond ((eq? (car args) (cadr args)) 't) (else '())) env]
     (swap)                    ; [env (cond ((eq? (car args) (cadr args)) 't) (else '()))]
     (dpop)                    ; [(cond ((eq? (car args) (cadr args)) 't) (else '()))]
     (return)

     (label IS_CAR?)           ; [func args env]
     (dup)                     ; [func func args env]
     (dpush
      (eq? (dpop) 'car))       ; [(eq? func 'car) func args env]
     (branch 'CAR 'IS_CDR?)    ; [func args env]

     (label CAR)               ; [func args env]
     (dpop)                    ; [args env]
     (dup)                     ; [args args env] ; for Scheme
     (dpush
      (cond
        ((null? (car (dpop)))
         '())
        (else
         (car
          (car (dpop))))))     ; [(car args) env]
     (swap)                    ; [env (car args)]
     (dpop)                    ; [(car args)]
     (return)

     (label IS_CDR?)           ; [func args env]
     (dup)                     ; [func func args env]
     (dpush
      (eq? (dpop) 'cdr))       ; [(eq? func 'cdr) func args env]
     (branch 'CDR 'IS_CONS?)   ; [func args env]

     (label CDR)               ; [func args env]
     (dpop)                    ; [args env]
     (dup)                     ; [args args env]  ; for Scheme
     (dpush
      (cond
        ((null? (car (dpop)))
         '())
        (else
         (cdr
          (car (dpop))))))     ; [(cdr args) env]
     (swap)                    ; [env (cdr args)]
     (dpop)                    ; [(cdr args)]
     (return)

     (label IS_CONS?)          ; [func args env]
     (dup)                         ; [func func args env]
     (dpush
      (eq? (dpop) 'cons))      ; [(eq? func 'cons) func args env]
     (branch 'CONS 'IS_COND?)  ; [func args env]

     (label CONS)              ; [func args env]
     (dpop)                    ; [args env]
     (dup)                     ; [args args env]
     (dpush
      (cons (car (dpop))
            (cadr (dpop))))    ; [(cons (car args) (cadr args)) env]
     (swap)                    ; [env (cons (car args) (cadr args))]
     (dpop)                    ; [(cons (car args) (cadr args))]
     (return)

     (label IS_COND?)          ; [func args env]
     (dup)                     ; [func func args env]
     (dpush
      (eq? (dpop) 'cond))      ; [(eq? func 'cond) func args env]
     (branch 'COND
             'IS_ERROR?)       ; [func args env]

     (label COND)              ; [func args env]
     (dpop)                    ; [args env]
     (jump 'evcond__)          ; [(evcond args env)]

     (label IS_ERROR?)         ; [func args env]
     (dup)                     ; [func func args env]
     (dpush
      (eq? (dpop) 'error))     ; [(eq? func 'error) func args env]
     (branch 'ERROR
             'FUNCTION)        ; [func args env]

     (label ERROR)             ; [func args env]
     (dpop)                    ; [args env]
     (dup)                     ; [args args env]
     (dpush
      (error (car (dpop))
             (cadr (dpop))))   ; [(error (car args) (cadr args)) env]
     (swap)                    ; [env (error (dpop) (dpop))]
     (dpop)                    ; [(error (dpop) (dpop))]
     (return)

     (label FUNCTION)          ; [func args env]
     (rot)                     ; [env func args]
     (dup)                     ; [env env func args]
     (rot)                     ; [func env env args]
     (dpush
      (assocv (dpop) (dpop)))  ; [(assocv func env) env args]
     (rot)                     ; [args (assocv func env) env]
     (swap)                    ; [(assocv func env) args env]
     (dpush
      (cons (dpop) (dpop)))    ; [(cons (assocv func env) args) env]
     (jump 'eval__)            ; [(eval (cons (assocv func env) args) env)]

     (label IS_LABEL?)         ; [func args env]
     (dup)                     ; [func func args env]
     (dpush
      (eq? (car (dpop))
           'label))            ; [(eq? (car func) 'label) func args env]
     (branch 'LABEL_
             'IS_LAMBDA?)      ; [func args env]

     (label LABEL_)            ; [func args env]
     (rot)                     ; [env func args]
     (over)                    ; [func env func args]
     (dup)                     ; [func func env func args]
     (dpush
      (cons
       (cons (cadr (dpop))
             (caddr (dpop)))
       (dpop)))                ; [(cons (cons (cadr func) (caddr func)) env) func args]
     (rot)                     ; [args (cons (cons (cadr func) (caddr func)) env) func]
     (rot)                     ; [func args (cons (cons (cadr func) (caddr func)) env)]
     (dpush
      (cons (caddr (dpop))
            (dpop)))           ; [(cons (caddr func) args) (cons (cons (cadr func) (caddr func)) env)]
     (jump 'eval__)            ; [(eval (cons (caddr func) args) (cons (cons (cadr func) (caddr func)) env))]

     (label IS_LAMBDA?)        ; [func args env]
     (dup)                     ; [func func args env]
     (dpush
      (eq? (car (dpop))
           'lambda))           ; [(eq? (car func) 'lambda) func args env]
     (branch 'LAMBDA
             'OTHER_FORM)      ; [func args env]

     (label LAMBDA)            ; [func args env]
     (dup)                     ; [func func args env]
     (rpush (dpop))            ; [func args env]
     (rot)                     ; [env func args]
     (rot)                     ; [args env func]
     (over)                    ; [env args env func]
     (swap)                    ; [args env env func]
     (dpush (rpop))            ; [func args env env func]
     (rot)                     ; [env func args env func]
     (rot)                     ; [args env func env func]
     (call 'evlist__
           'LAMBDA_2)          ; [(evlist args env) func env func]

     (label LAMBDA_2)          ; [(evlist args env) func env func]
     (swap)                    ; [func (evlist args env) env func]
     (dpush
      (append
       (assoclist
        (cadr (dpop)) (dpop))
       (dpop)))                ; [(append (assoclist (cadr func) (evlist args env)) env) func]
     (swap)                    ; [func (append (assoclist (cadr func) (evlist args env)) env)]
     (dpush (caddr (dpop)))    ; [(caddr func) (append (assoclist (cadr func) (evlist args env)) env)]
     (jump 'eval__)            ; [(eval (caddr func) (append (assoclist (cadr func) (evlist args env)) env))]

     (label OTHER_FORM)        ; [func args env]
     (dpush
      (error
       '2
       (cons (dpop) (dpop))))  ; [(error '2 (cons func args)) env]
     (swap)                    ; [env (error '2 (cons func args))]
     (dpop)                    ; [(error '2 (cons func args))]
     (return))))


(define evcond__
  (prog<-flow
   '((label START)    ; dstack = [clauses env]
     (dup)                     ; [clauses caluses env]
     (dpush (null? (dpop)))    ; [(null? clauses) clauses env]
     (branch 'EMPTY
             'CONDITION)       ; [cluses env]

     (label EMPTY)             ; [clauses env]
     (dpop)                    ; [env]
     (dpop)                    ; []
     (dpush '())               ; [()]
     (return)

     (label CONDITION)         ; [clauses env]
     (over)                    ; [env clauses env]
     (over)                    ; [clauses env clauses env]
     (dpush (caar (dpop)))     ; [(caar clauses) env clauses env]
     (call 'eval__
           'IS_FALSE?)         ; [(eval (caar clauses) env) clauses env]

     (label IS_FALSE?)         ; [(eval (caar clauses) env) clauses env]
     (dpush (null? (dpop)))    ; [(null? (eval (caar clauses) env)) clauses env]
     (branch 'NEXT 'EXECUTE)   ; [clauses env]

     (label NEXT)              ; [clauses env]
     (dpush (cdr (dpop)))      ; [(cdr clauses) env]
     (jump 'evcond__)          ; [(evcond (cdr cluases)) env]

     (label EXECUTE)           ; [clauses env]
     (dpush (cadar (dpop)))    ; [(cadar clauses) env]
     (jump 'eval__)            ; [(eval (cadar clause)) env]
     )))


(define evlist__
  (prog<-flow
   '((label START)    ; dstack = [members env]
     (dup)                     ; [members members env]
     (dpush (null? (dpop)))    ; [(null? members) members env]
     (branch 'EMPTY 'NEXT)     ; [members env]

     (label EMPTY)             ; [members env]
     (dpop)                    ; [env]
     (dpop)                    ; []
     (dpush '())               ; [()]
     (return)

     (label NEXT)              ; [members env]
     (over)                    ; [env members env]
     (over)                    ; [members env members env]
     (dpush (cdr (dpop)))      ; [(cdr members) env members env]
     (call 'evlist__ 'EVAL)    ; [(evlist (cdr members) env) members env]

     (label EVAL)              ; [(evlist (cdr members) env) members env]
     (rot)                     ; [env (evlist (cdr members) env) members]
     (rot)                     ; [members env (evlist (cdr members) env)]
     (dpush (car (dpop)))      ; [(car members) env (evlist (cdr members) env)]
     (call 'eval__ 'CONS)      ; [(eval (car members) env) (evlist (cdr members) env)]

     (label CONS)              ; [(eval (car members) env) (evlist (cdr members) env)]
     (dpush
      (cons (dpop) (dpop)))    ; [(cons (eval (car members) env) (evlist (cdr members) env))]
     (return))))

; =====================================
