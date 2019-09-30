# miniCUTE-g-machine-syntax

[![License](https://img.shields.io/github/license/CUTE-Lang/miniCUTE.svg)](https://github.com/CUTE-Lang/miniCUTE/blob/master/LICENSE)
[![CircleCI](https://circleci.com/gh/CUTE-Lang/miniCUTE/tree/master.svg?style=svg)](https://circleci.com/gh/CUTE-Lang/miniCUTE/tree/master)
[![Repository size](https://img.shields.io/github/repo-size/CUTE-Lang/miniCUTE.svg)](https://github.com/CUTE-Lang/miniCUTE/)
[![GitHub issues](https://img.shields.io/github/issues-raw/CUTE-Lang/miniCUTE.svg)](https://github.com/CUTE-Lang/miniCUTE/issues)
[![GitHub PRs](https://img.shields.io/github/issues-pr-raw/CUTE-Lang/miniCUTE.svg)](https://github.com/CUTE-Lang/miniCUTE/pull)
[![GitHub last commit date to master](https://img.shields.io/github/last-commit/CUTE-Lang/miniCUTE/master.svg)](https://github.com/CUTE-Lang/miniCUTE/commits/master)

Types and functions for G-machine syntax.

# EBNF syntax

``` ebnf
program = { function definition, whitespace };

function definition = identifier, whitespace, "<", integer, ">", whitespace, instructions;

instructions = "{", { instruction, ";", whitespace }, "}";
instruction = "MakeInteger", integer
            | "MakeConstructor", integer, integer
            | "MakeStructure", integer, integer
            | "MakeApplication",
            | "MakeGlobal", identifier
            | "MakePlaceholders", integer

            | "Pop" integer
            | "Dig" integer
            | "Update" integer
            | "Copy" integer

            | "PushBasicValue", integer
            | "PushExtractedValue"
            | "WrapAsInteger"
            | "WrapAsStructure"
            | "UpdateAsInteger", integer
            | "UpdateAsStructure", integer

            | "Primitive", primitive operator

            | "Unwind"
            | "Destruct", integer

            | "Eval"
            | "Return"

            | "Match", match table;

match table = "{", { match entry }, "}";
match entry = integer, "->", instructions;

primitive operators = "+"
                    | "-"
                    | "*"
                    | "/"
```
