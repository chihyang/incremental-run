An incremental `run`
==

This is the complete implementation and experimental code of our submission to
TFP2024.  The files are:

- [`mk-function-run`](mk-function-run.rkt)/[`demo-function-run`](demo-function-run.rkt):
  the implementation of `run` that returns a function taking one nonnegative
  integer and the demo of using this implementation.
- [`mk-noncomposable-run`](mk-noncomposable-run.rkt)/[`demo-noncomposable-run`](demo-noncomposable-run.rkt):
  the implementation of `run` that returns a one argument relation, but the
  returned relation is not composable, in the sense that when this relation is
  put at different positions in a `conj`, we get different results.
- [`mk-composable-run`](mk-composable-run.rkt)/[`demo-composable-run`](demo-composable-run.rkt):
  the implementation of `run` that returns a one argument relation, and the
  returned relation is composable, in the sense that no matter where this
  relation is put in a `conj`, we always get the same result.

