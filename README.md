# plisp
- [plisp](#plisp)
  - [Overview](#overview)
  - [Note](#note)
  - [Usage](#usage)

## Overview

'plisp' is the environment list in which a meta-circular evaluator is written.

If 'eval' in a plisp is implemented to take a plisp as an environment list, it can execute itself.

Although Scheme is used only for that implementation, the contents of plisp do not depend on Scheme.

Furthermore, if 'plisp' is applied "complete self-embedding" <del>(There is an example in plisp.rkt)</del>, unlimited nesting of evals would in principle be possible.
(P.S. I changed it to "incomplete self-embedding" because not all LISP interpretor can easily handle infinite lists, and moreover, the above way of achieving "infinite nesting of evals" is inefficient.)

## Note

It is assumed that this will be run on DrRacket.

If your DrRacket language mode is "R5RS", remove "#lang sicp" from the beginning of the file.

## Usage

We assume you are using the DrRacket interpreter.

    > (repl)⏎
"(repl)" will start a virtual REPL.

    >>
'>>' is the virtual REPL prompt.

    >> (importenv plisp2)⏎
    t

Import "plisp2" into the grobal environment list "\*env\*".
(You can use "(exportenv)" to output the S-expression of the contents of "\*env\*".)
The 't' is the boolean true, but it's just a dummy to return something.

Of course, you could import "plisp" instead of "plisp2" (but in that case the steps below would not work).

    >>⏎
    (def reverse_z
      '(lambda (l)
          (funcall
           (z-combi
            (function (lambda (f)
                (function (lambda (l acc)
                    (cond
                      ((eq '() l) acc)
                      (t (f (cdr l)
                            (cons (car l) acc)))))))))
           l '())))⏎
    reverse_z

The function "reverse_z" is now defined.

    >> (eval '(reverse_z '(1 2 3 4 5 6 7 8 9 10)) *env*)⏎
    (10 9 8 7 6 5 4 3 2 1)

Here, if we try to evaluate "reverse_z" directly, we get an error.
This is because the virtual REPL uses "eval_" which does not know "function", which is a reserved word for "eval" in "plips2".
To do this, we must evaluate "reverse_z" using "eval" defined in "plisp2" (which is imported into "\*env\*").

Exit the virtual REPL.

    >> (exit)
    >
