# miniCUTE-minicute-syntax

[![License](https://img.shields.io/github/license/CUTE-Lang/miniCUTE.svg)](https://github.com/CUTE-Lang/miniCUTE/blob/master/LICENSE)
[![CircleCI](https://circleci.com/gh/CUTE-Lang/miniCUTE/tree/master.svg?style=svg)](https://circleci.com/gh/CUTE-Lang/miniCUTE/tree/master)
[![Repository size](https://img.shields.io/github/repo-size/CUTE-Lang/miniCUTE.svg)](https://github.com/CUTE-Lang/miniCUTE/)
[![GitHub issues](https://img.shields.io/github/issues-raw/CUTE-Lang/miniCUTE.svg)](https://github.com/CUTE-Lang/miniCUTE/issues)
[![GitHub PRs](https://img.shields.io/github/issues-pr-raw/CUTE-Lang/miniCUTE.svg)](https://github.com/CUTE-Lang/miniCUTE/pull)
[![GitHub last commit date to master](https://img.shields.io/github/last-commit/CUTE-Lang/miniCUTE/master.svg)](https://github.com/CUTE-Lang/miniCUTE/commits/master)

Types and functions for miniCUTE syntax.

# EBNF syntax

Note: `white space`, `identifier`, and `integer` are not defined in the following EBNF definitions.

```ebnf
program = { top level definition, whitespace };

top level definition = supercombinator; (* other top level definitions will be added in future *)

supercombinator = identifier, whitespace, { identifier, whitespace }, "=", whitespace, expression, ";";

expression = let expression
           | match expression
           | lambda expression
           | precedence expression;

let expression = ( "letrec" | "let" ), whitespace, let definitions, whitespace, "in", whitespace, expression;
let definitions = let definition, { ";", whitespace, let definition }, ( ";" );
let definition = identifier, whitespace, "=", whitespace, expression;

match expression = "match", whitespace, expression, whitespace, "with", match cases;
match cases = match case, { ";", whitespace, match case };
match case = "<", integer, ">", whitespace, { identifier, whitespace }, "->", whitespace, expression;

lambda expression = "\", identifier, whitespace, { identifier, whitespace }, "->", whitespace, expression;

(* The following non terminals depend on precedence table *)
precedence expression = precedence 100 expression;
precedence 100 expression = (* application *) precedence 100 expression, whitespace, precedence 99 expression
                          | precedence 99 expression;
precedence 99 expression = precedence 98 expression;
(*         ...                        ...          *)
precedence 51 expression = precedence 50 expression;
precedence 50 expression = (* multiplication *) precedence 50 expression,  "*", precedence 49 expression
                         | (* division *) precedence 50 expression,  "/", precedence 49 expression
                         | precedence 49 expression;
precedence 49 expression = precedence 48 expression;
(*         ...                        ...          *)
precedence 41 expression = precedence 40 expression;
precedence 40 expression = (* addition *) precedence 40 expression,  "+", precedence 39 expression
                         | (* subtraction *) precedence 40 expression,  "-", precedence 39 expression
                         | precedence 39 expression;
precedence 39 expression = precedence 38 expression;
(*         ...                        ...          *)
precedence 1 expression = precedence 0 expression;
precedence 0 expression = atomic expression;

atomic expression = integer expression
                  | variable expression
                  | constructor expression
                  | "(", expression, ")";

integer expression = integer;
variable expression = identifier;
constructor expression = "$C{", integer, ";", integer, "}";
```
