# `continuations`

This is a Haskell package defining continuations modelled as functions. A variety of related functionality is provided, applying continuations to eliminators for conjunctions and disjunctions, contravariant mapping of functions in CPS, a contravariant applicative class taking apart sums in the same manner as `Applicative` builds products, co-functions (the dual of functions, consisting of an argument and a continuation from the return), and more.

Contravariance is a major theme here; most of the functionality of the package is designed for arbitrary contravariant functors. (Think continuations, predicates, relations, pretty-printers, or the left type parameter of profunctors.)
