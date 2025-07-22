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
;; An Example of "Program"
;
; ((START <S-exp> ... <S-exp> (branch 'Name1 'Name2))
;  (Name1 <S-exp> ... <S-exp> (return))
;  (Name2 <S-exp> ... <S-exp> (goto 'START)))
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
(define *PC* '(()))

; data stack
(define *dstack* '())

; return stack
(define *rstack* '())

; push to the data stack
(define (dpush v)
  (set! *dstack* (cons v *dstack*)))

; push to the return stack
(define (rpush v)
  (set! *rstack* (cons v *rstack*)))

; pop from the data stack
(define (dpop)
  (let ((t (car *dstack*))) (set! *dstack* (cdr *dstack*)) t))

; pop from the return stack
(define (rpop)
  (let ((t (car *rstack*))) (set! *rstack* (cdr *rstack*)) t))

; goto
(define (goto label)
  (set! *PC* (cons label (cdr *PC*))))

; branch
(define (branch yes no)
  (goto (if (dpop) yes no)))

; return
(define (return)
  (set! *PC* (rpop)))

; call
(define (call func return_label)
  (rpush (cons return_label (cdr *PC*)))
  (set! *PC* (cons 'START func)))

; jump
(define (jump func)
  (set! *PC* (cons 'START func))
  (run))

(define (my_eval exp)
  (eval exp (interaction-environment)))

(define (exec terms)
  (cond ((atom? terms) 'ok)
        (else
         (my_eval (car terms))
         (exec (cdr terms)))))

(define (run)
  (cond ((eq? '() (car *PC*)) 'ok)
        (else
         (exec (cdr (assoc* (car *PC*)
                            (my_eval (cdr *PC*)))))
         (run))))

(define (do func)
  (call func '()) (run) (dpop))

;; Stack Operations

(define (dup)
  (set! *dstack* (cons (car *dstack*) *dstack*)))

(define (over)
  (set! *dstack* (cons (cadr *dstack*) *dstack*)))

(define (swap)
  (let ((t (dpop)) (s (dpop))) (dpush t) (dpush s)))

(define (rot)
  (let ((top (dpop)) (sec (dpop)) (thir (dpop)))
    (dpush sec) (dpush top) (dpush thir)))

(define (.S) (display *dstack*) (newline))

; clear stacks and program counter
(define (clearsys)
  (set! *dstack* '())
  (set! *rstack* '())
  (set! *PC* '(())))

;; Stack Version "eval"

(define (eval_s exp env)
  (clearsys)
  (dpush env)
  (dpush exp)
  (do 'eval__))

; =====================================
;; Pure LISP Interpreter for Stack and GOTO System
; Note!
; Assume that the order of evaluation of arguments in a Scheme interpretor is left to right.

(define eval__
  (prog<-flow
   '((label START)  ; *dstack* = [e a]
     (dup)                     ; [e e a]
     (dpush (eq? (dpop) 't))   ; [(eq? e 't) e a]
     (branch 'T 'IS_NIL?)      ; [e a]

     (label T)                 ; [e a]
     (dpop)                    ; [a]
     (dpop)                    ; []
     (dpush 't)                ; [t]
     (return)

     (label IS_NIL?)           ; [e a]
     (dup)                     ; [e e a]
     (dpush (eq? (dpop) '()))  ; [(eq? e '()) e a]
     (branch 'NIL 'IS_ATOM?)   ; [e a]

     (label NIL)               ; [e a]
     (dpop)                    ; [a]
     (dpop)                    ; []
     (dpush '())               ; [()]
     (return)

     (label IS_ATOM?)          ; [e a]
     (dup)                     ; [e e a]
     (dpush (atom? (dpop)))    ; [(atom? e) e a]
     (branch 'ATOM 'IS_SUBR?)  ; [e a]

     (label ATOM)              ; [e a]
     (dpush
      (cdr (assoc*
            (dpop) (dpop))))   ; [(cdr (assoc* e a))]
     (return)

     (label IS_SUBR?)          ; [e a]
     (dup)                     ; [e e a]
     (dpush
      (isSUBR? (car (dpop))))  ; [(isSUBR? (car e)) e a]
     (branch 'APPLY_EV
             'IS_FSUBR?)       ; [e a]

     (label IS_FSUBR?)         ; [e a]
     (dup)                     ; [e e a]
     (dpush
      (atom? (car (dpop))))    ; [(atom? (car e)) e a]
     (branch 'APPLY
             'IS_LAMBDA?)      ; [e a]

     (label IS_LAMBDA?)        ; [e a]
     (dup)                     ; [e e a]
     (dpush
      (eq? (caar (dpop))
           'lambda))           ; [(eq? (caar e) 'lambda) e a]
     (branch 'APPLY_EV
             'APPLY)           ; [e a]

     (label APPLY)             ; [e a]
     (dup)                     ; [e e a]
     (dpush (cdr (dpop)))      ; [(cdr e) e a]
     (swap)                    ; [e (cdr e) a]
     (dpush (car (dpop)))      ; [(car e) (cdr e) a]
     (jump 'apply__)           ; [(apply (car e) (cdr e) a)]

     (label APPLY_EV)          ; [e a]
     (over)                    ; [a e a]
     (over)                    ; [e a e a]
     (dpush (cdr (dpop)))      ; [(cdr e) a e a]
     (call 'evlis__
           'APPLY_EV_2)        ; [(evlis (cdr e) a) e a]

     (label APPLY_EV_2)        ; [(evlis (cdr e) a) e a]
     (swap)                    ; [e (evlis (cdr e) a) a]
     (dpush (car (dpop)))      ; [(car e) (evlis (cdr e) a) a]
     (jump 'apply__))))        ; [(apply (car e) (evlis (cdr e) a) a)]


(define apply__
  (prog<-flow
   '((label START)  ; *dstack* = [fn args a]
     (dup)                     ; [fn fn args a]
     (dpush (atom? (dpop)))    ; [(atom? fn) fn args a]
     (branch 'IS_QUOTE?
             'IS_LABEL?)       ; [fn args a]

     (label IS_QUOTE?)         ; [fn args a]
     (dup)                     ; [fn fn args a]
     (dpush
      (eq? (dpop) 'quote))     ; [(eq? fn 'quote) fn args a]
     (branch 'QUOTE
             'IS_ATOM?)        ; [fn args a]

     (label QUOTE)             ; [fn args a]
     (dpop)                    ; [args a]
     (dpush (car (dpop)))      ; [(car args) a]
     (swap)                    ; [a (car args)]
     (dpop)                    ; [(car args)]
     (return)

     (label IS_ATOM?)          ; [fn args a]
     (dup)                     ; [fn fn args a]
     (dpush
      (eq? (dpop) 'atom))      ; [(eq? fn 'atom) fn args a]
     (branch 'ATOM 'IS_EQ?)    ; [fn args a]

     (label ATOM)              ; [fn args a]
     (dpop)                    ; [args a]
     (dpush
      (atom (car (dpop))))     ; [(atom (car args)) a]
     (swap)                    ; [a (atom (car args))]
     (dpop)                    ; [(atom (car args))]
     (return)

     (label IS_EQ?)            ; [fn args a]
     (dup)                     ; [fn fn args a]
     (dpush (eq? (dpop) 'eq))  ; [(eq? fn 'eq) fn args a]
     (branch 'EQ 'IS_CAR?)     ; [fn args a]

     (label EQ)                ; [fn args a]
     (dpop)                    ; [args a]
     (dup)                     ; [args args a]
     (dpush
      (eq (car (dpop))
          (cadr (dpop))))      ; [(eq (car args) (cadr args)) a]
     (swap)                    ; [a (eq (car args) (cadr args))]
     (dpop)                    ; [(eq (car args) (cadr args))]
     (return)

     (label IS_CAR?)           ; [fn args a]
     (dup)                     ; [fn fn args a]
     (dpush
      (eq? (dpop) 'car))       ; [(eq? fn 'car) fn args a]
     (branch 'CAR 'IS_CDR?)    ; [fn args a]

     (label CAR)               ; [fn args a]
     (dpop)                    ; [args a]
     (dpush
      (car (car (dpop))))      ; [(car (car args)) a]
     (swap)                    ; [a (car (car args))]
     (dpop)                    ; [(car (car args))]
     (return)

     (label IS_CDR?)           ; [fn args a]
     (dup)                     ; [fn fn args a]
     (dpush
      (eq? (dpop) 'cdr))       ; [(eq? fn 'cdr) fn args a]
     (branch 'CDR 'IS_CONS?)   ; [fn args a]

     (label CDR)               ; [fn args a]
     (dpop)                    ; [args a]
     (dpush
      (cdr (car (dpop))))      ; [(cdr (car args)) a]
     (swap)                    ; [a (cdr (car args))]
     (dpop)                    ; [(cdr (car args))]
     (return)

     (label IS_CONS?)          ; [fn args a]
     (dup)                     ; [fn fn args a]
     (dpush
      (eq? (dpop) 'cons))      ; [(eq? fn 'cons) fn args a]
     (branch 'CONS 'IS_COND?)  ; [fn args a]

     (label CONS)              ; [fn args a]
     (dpop)                    ; [args a]
     (dup)                     ; [args args a]
     (dpush
      (cons (car (dpop))
            (cadr (dpop))))    ; [(cons (car args) (cadr args)) a]
     (swap)                    ; [a (cons (car args) (cadr args))]
     (dpop)                    ; [(cons (car args) (cadr args))]
     (return)

     (label IS_COND?)          ; [fn args a]
     (dup)                     ; [fn fn args a]
     (dpush
      (eq? (dpop) 'cond))      ; [(eq? fn 'cond) fn args a]
     (branch 'COND
             'IS_ERROR?)       ; [fn args a]

     (label COND)              ; [fn args a]
     (dpop)                    ; [args a]
     (jump 'evcon__)           ; [(evcon args a)]

     (label IS_ERROR?)         ; [fn args a]
     (dup)                     ; [fn fn args a]
     (dpush
      (eq? (dpop) 'error))     ; [(eq? fn 'error) fn args a]
     (branch 'ERROR
             'FUNCTION)        ; [fn args a]

     (label ERROR)             ; [fn args a]
     (dpop)                    ; [args a]
     (dup)                     ; [args args a]
     (dpush
      (error (car (dpop))
             (cadr (dpop))))   ; [(error (car args) (cadr args)) a]
     (swap)                    ; [a (error (dpop) (dpop))]
     (dpop)                    ; [(error (dpop) (dpop))]
     (return)

     (label FUNCTION)          ; [fn args a]
     (rot)                     ; [a fn args]
     (dup)                     ; [a a fn args]
     (rot)                     ; [fn a a args]
     (dpush
      (cdr (assoc*
            (dpop) (dpop))))   ; [(cdr (assoc* fn a)) a args]
     (rot)                     ; [args (cdr (assoc* fn a)) a]
     (swap)                    ; [(cdr (assoc* fn a)) args a]
     (dpush
      (cons (dpop) (dpop)))    ; [(cons (cdr (assoc* fn a)) args) a]
     (jump 'eval__)            ; [(eval (cons (cdr (assoc* fn a)) args) a)]

     (label IS_LABEL?)         ; [fn args a]
     (dup)                     ; [fn fn args a]
     (dpush
      (eq? (car (dpop))
           'label))            ; [(eq? (car fn) 'label) fn args a]
     (branch 'LABEL_
             'IS_LAMBDA?)      ; [fn args a]

     (label LABEL_)            ; [fn args a]
     (rot)                     ; [a fn args]
     (over)                    ; [fn a fn args]
     (dup)                     ; [fn fn a fn args]
     (dpush
      (cons
       (cons (cadr (dpop))
             (caddr (dpop)))
       (dpop)))                ; [(cons (cons (cadr fn) (caddr fn)) a) fn args]
     (rot)                     ; [args (cons (cons (cadr fn) (caddr fn)) a) fn]
     (rot)                     ; [fn args (cons (cons (cadr fn) (caddr fn)) a)]
     (dpush
      (cons (caddr (dpop))
            (dpop)))           ; [(cons (caddr fn) args) (cons (cons (cadr fn) (caddr fn)) a)]
     (jump 'eval__)            ; [(eval (cons (caddr fn) args) (cons (cons (cadr fn) (caddr fn)) a))]

     (label IS_LAMBDA?)        ; [fn args a]
     (dup)                     ; [fn fn args a]
     (dpush
      (eq? (car (dpop))
           'lambda))           ; [(eq? (car fn) 'lambda) fn args a]
     (branch 'LAMBDA
             'OTHER_FORM)      ; [fn args a]

     (label LAMBDA)            ; [fn args a]
     (dup)                     ; [fn fn args a]
     (rpush (dpop))            ; [fn args a]
     (rot)                     ; [a fn args]
     (rot)                     ; [args a fn]
     (dpush (rpop))            ; [fn args a fn]
     (dpush
      (pairlis*
       (cadr (dpop)) (dpop)
       (dpop)))                ; [(pairlis* (cadr fn) args a) fn]
     (swap)                    ; [fn (pairlis* (cadr fn) args a)]
     (dpush (caddr (dpop)))    ; [(caddr fn) (pairlis* (cadr fn) args a)]
     (jump 'eval__)            ; [(eval (caddr fn) (pairlis* (cadr fn) args a))]

     (label OTHER_FORM)        ; [fn args a]
     (dpush
      (error
       '2
       (cons (dpop) (dpop))))  ; [(error '2 (cons fn args)) a]
     (swap)                    ; [a (error '2 (cons fn args))]
     (dpop)                    ; [(error '2 (cons fn args))]
     (return))))


(define evcon__
  (prog<-flow
   '((label START)  ; *dstack* = [c a]
     (over)                    ; [a c a]
     (over)                    ; [c a c a]
     (dpush (caar (dpop)))     ; [(caar c) a c a]
     (call 'eval__
           'IS_FALSE?)         ; [(eval (caar c) a) c a]

     (label IS_FALSE?)         ; [(eval (caar c) a) c a]
     (dpush (null? (dpop)))    ; [(null? (eval (caar c) a)) c a]
     (branch 'NEXT 'EXECUTE)   ; [c a]

     (label NEXT)              ; [c a]
     (dpush (cdr (dpop)))      ; [(cdr c) a]
     (jump 'evcon__)           ; [(evcon (cdr c)) a]

     (label EXECUTE)           ; [c a]
     (dpush (cadar (dpop)))    ; [(cadar c) a]
     (jump 'eval__)            ; [(eval (cadar c)) a]
     )))


(define evlis__
  (prog<-flow
   '((label START)  ; *dstack* = [m a]
     (dup)                     ; [m m a]
     (dpush (null? (dpop)))    ; [(null? m) m a]
     (branch 'EMPTY 'NEXT)     ; [m a]

     (label EMPTY)             ; [m a]
     (dpop)                    ; [a]
     (dpop)                    ; []
     (dpush '())               ; [()]
     (return)

     (label NEXT)              ; [m a]
     (over)                    ; [a m a]
     (over)                    ; [m a m a]
     (dpush (cdr (dpop)))      ; [(cdr m) a m a]
     (call 'evlis__ 'EVAL)     ; [(evlis (cdr m) a) m a]

     (label EVAL)              ; [(evlis (cdr m) a) m a]
     (rot)                     ; [a (evlis (cdr m) a) m]
     (rot)                     ; [m a (evlis (cdr m) a)]
     (dpush (car (dpop)))      ; [(car m) a (evlis (cdr m) a)]
     (call 'eval__ 'CONS)      ; [(eval (car m) a) (evlis (cdr m) a)]

     (label CONS)              ; [(eval (car m) a) (evlis (cdr m) a)]
     (dpush
      (cons (dpop) (dpop)))    ; [(cons (eval (car m) a) (evlis (cdr m) a))]
     (return))))
