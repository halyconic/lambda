# lambda
Various lambda calculus interpreters

## 1: Standard interpreter

This interpreter accurately encodes the [reduction rules](https://en.wikipedia.org/wiki/Lambda_calculus#Reduction) of lambda calculus. The interpreter operates on the AST directly.

## 2: Interpreter using fixed point functions

This interpreter uses [catamorphisms](https://en.wikipedia.org/wiki/Catamorphism) and [anamorphisms](https://en.wikipedia.org/wiki/Anamorphism) to encode the reduction rules by using fixed points. This makes the interpreter more efficient. The interpreter operates on the AST directly, but a naive writer is available to view fixed point expressions.

## 3: Interpreter using De Bruijn Indices

This interpreter simplifies the naming rules of lambda calculus, using [De Bruijn indices](https://en.wikipedia.org/wiki/De_Bruijn_index). The interpreter operates on the AST directly, but a naive writer is available to view fixed point expressions.
