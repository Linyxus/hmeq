# Implementation of HM(=)

This is a simple implementation of HM(=), the specialized version of HM(x)
proposed in *Advanced topics in types and programming languages, Chapter 10*.

## Syntax

  x, y ::=                 identifiers
           z               variable
           c               constant

  t    ::=                 expressions
           x               identifier
           \z. t           abstraction
           t t             application
           let z = t in t  let

  v, w ::=                 values
           z               variable
           \z. t           function
           C v1 ... vk     data
           D v1 ... vk     partial elimination
