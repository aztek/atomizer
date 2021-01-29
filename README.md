Atomizer
===

[![Build Status](https://travis-ci.org/aztek/atomizer.svg?branch=master)](https://travis-ci.org/aztek/atomizer)

Atomizer is an experimental static analysis tool for Erlang programs.

The Erlang compiler can warn you if you make a typo in a function name or a variable. Atomizer is an experiment in creating a tool that can warn about typos in Erlang atoms. [Dialyzer](https://erlang.org/doc/man/dialyzer.html) might be able to detect misfit atoms in some cases, but its powers are limited. For example, it can never find a misfit atom in a pattern matching.

Given an Erlang program, atomizer does the following.

1. Parse all `*.erl` files and extract all the atoms from them.
2. Compute the *normal form* of each atom.
3. Find all pairs of distinct atoms with the same normal form.
4. Filter out all pairs of distinct atoms that are likely false positives.
5. For each pair of distinct atoms guess which of them is likely the typo.

The most important parts of this algorithm, i.e. computation of the normal form, filtering false positives and guessing the typo, are heuristical. Finding a satisfying balance between sensitivity and precision for them is a challenge. The current implementation of the heuristics considers an atom misfit if it is spelled with an outlier casing, e.g. `ip_address` instead of `ipAddress`.

An atom is in the normal form when it is all lowercase and does not contain dashes and underscores. This definition ensures that atoms spelled in camelCase, snake_case, SCREAMING_SNAKE_CASE, kebab-case and their combination all have the same normal form.

A pair of distinct atoms with matching normal forms is only reported if all of the following is true.

- One of the atoms occurs in the program at least *n* times more often than the other (*n* is arbitrarily chosen to be 4).
- The atom with fewer occurrences only occurs in a single `.erl` file.
- The atom with more occurrences occurs in the same file as the other one.

In a pair of distinct atoms with matching normal forms, the atom with the fewest occurrences in the program is considered to be the typo.

Atomizer is designed to be run on a large codebase. It utilizes concurrency for traversing the file system, parsing, computing normal forms and filtering.

Atomizer with the above choice of heuristics was able to find several bugs in a large (5MLOC) legacy Erlang application at Ericsson. The success was largely due to the fact that the application included a lot of machine-generated Erlang code from ASN.1 definitions. This generated code had a hodgepodge of naming conventions in the atoms that spead to the rest of the codebase.

Compile with `make`, run `./bin/atomizer`.
