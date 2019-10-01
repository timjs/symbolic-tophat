Symbolic execution for Task Oriented Programming.
Article accepted for publication and presentation at [IFL'19](http://2019.iflconference.org).

## Abstract

Task-Oriented Programming (TOP) is a programming paradigm that allows declarative specification of workflows.
TOP is typically used in domains where functional correctness is essential, and where failure can have financial or strategical consequences.
In this paper we aim to make formal verification of software written in TOP easier.
Currently, only testing is used to verify that programs behave as intended.
We use symbolic execution to guarantee that no aberrant behaviour can occur.
In previous work we presented TopHat, a formal language that implements the core aspects of TOP.
In this paper we develop a symbolic execution semantics for TopHat.
Symbolic execution allows to prove that a given property holds for all possible execution paths of TopHat programs.

We show that the symbolic execution semantics is consistent with the original TopHat semantics, by proving soundness and completeness.
We present an implementation of the symbolic execution semantics in Haskell.
By running example programs, we validate our approach.
This work represents a step forward in the formal verification of TOP software.

## Article

Can be found in [main.pdf](main.pdf).

## Implementation

A Haskell implementation can be found in a [separate repository](https://github.com/timjs/symbolic-tophat-haskell).
