## What is it?

This is a Clojure based implementation of a logical programming language from [SICP](https://web.mit.edu/6.001/6.037/sicp.pdf) (subsection 4.4 "Logic Programming").

## Prerequisites

One needs to have Clojure and Leiningen preinstalled on your machine

## How can one run it?

To run the driver loop processing queries:

1. Navigate to the root directory of the repository
2. Execute the command: `lein run path/to/database`
   
Where `database` is a file containing a collection of assertions and rules (see Database Examples section).

## Database Examples

The `./examples` directory contains sample databases, primarily derived from SICP. One can practice excersizes from SICP with these databases.


## Difference from the Scheme based implementation from SICP

A primary divergence from the reference SICP implementation concerns the `lisp-value` special form. `lisp-value` takes a quoted Clojure from specifying a predicate, e.g. `"#(< % 20)"`, while the reference implementation takes unquoted Scheme form.

## TODO
1. Loops detection in deduction chains (see Exercise 4.67 from SICP);
2. The `and` special form performance optimization outlined in Exercise 4.76 of SICP.
