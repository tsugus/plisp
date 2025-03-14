# plisp

## overview

plisp' is the environment list in which a meta-circular evaluator is written.

If 'eval' in a plisp is implemented to take a plisp as an environment list, it can execute itself.

Although Scheme is used only for that implementation, the contents of plisp do not depend on Scheme.

Furthermore, if 'plisp' is applied "complete self-embedding" (There is an example in plisp.rkt), unlimited nesting of evals would in principle be possible.

## note

It is assumed that this will be run on Dr.Racket.

Please delete "#lamg ..." from the first line of plisp.rkt depending on your Scheme processing system.
