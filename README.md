# plisp
- [plisp](#plisp)
  - [Overview](#overview)
  - [Note](#note)
  - [Examples](#examples)
    - [Example 1](#example-1)
    - [Example 2](#example-2)

## Overview

"plisp" is the environment list in which a meta-circular evaluator is written.

If "eval" in a plisp is implemented to take a plisp as an environment list, it can execute itself.

Although Scheme is used only for that implementation, the contents of plisp do not depend on Scheme.

## Note

I have confirmed that it works in DrRacket's R5RS mode.

## Examples

First, run "main.scm", please.

### Example 1

    > (repl)⏎
    t
    (def reverse
      '(lambda (x)
         ((label
           rec
           (lambda (x y)
             (cond ((eq '() x) y)
                   (t (rec (cdr x) (cons (car x) y))))))
          x '())))⏎
    reverse
    (reverse '(1 2 3 4 5 6 7 8 9 10))⏎
    (10 9 8 7 6 5 4 3 2 1)
    (exit)⏎
    > (repl 'eval_s)
    (reverse '(1 2 3 4 5 6 7 8 9 10))⏎
    (10 9 8 7 6 5 4 3 2 1)
    (exit)⏎
    >

"eval_s" is defined in "slisp.scm".
"(repl)" is equivalent to "(repl 'eval_)".

### Example 2

    >(repl)⏎
    (importenv plisp)⏎
    t
    (exportenv)⏎
    ((*env* (caar lambda (x) (car (car x))) (cadr lambda (x) (car (cdr x))) ......))
    (resetenv)⏎
    t
    (exportenv)⏎
    ((*env*))
    *env*⏎
    ()
    (exit)⏎
    > *env*⏎
    ((*env*))
