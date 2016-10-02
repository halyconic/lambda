# lambda
Various lambda calculus interpreters

## 1: Standard interpreter

This interpreter accurately encodes the reduction rules as written [here](https://en.wikipedia.org/wiki/Lambda_calculus#Reduction). The interpreter operates on the AST directly.

## 2: Interpreter using fixed point functions

This interpreter uses catamorphisms and anamorphisms to encode the reduction rules by using fixed points. This makes the interpreter more efficient. The interpreter operates on the AST directly, but a naive writer is available to view fixed point expressions.
