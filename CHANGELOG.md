# Changelog for `Lambda-Parse-and-Play`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [0.2.0] – 2025-07-15

### Added
- `letrec` syntax and semantics for recursive bindings (e.g. factorial).
- Support for infinite recursion detection in `:trace` (500-step limit with error message).
- Parser now handles `letrec` and pretty-printer outputs correct syntax.
- Demo updated to include `letrec` usage with factorial.
- Full trace demonstration via `:trace` with reduction steps.
- Pretty-printer demonstration via `:pretty`.
- Error examples in demo for invalid syntax and unbound variables.

### Changed
- Evaluation function now supports `LetRec` in addition to `Let`, `If`, and function application.
- Trace engine (`traceExpr`) improved for stability on recursive expressions.

### Fixed
- Removed incomplete pattern match warning in substitution by adding support for `LetRec`.

---

## [0.1.0] – 2025-07-10

### Added
- Core Lambda Calculus interpreter with:
  - `Var`, `Lam`, `App`, `Lit`, `Prim`, `Let`, `If`.
- REPL with commands:
  - `:quit` — Exit the REPL.
  - `:load <file>` — Load and evaluate expression from file.
  - `:trace <expr>` — Show step-by-step beta/delta reduction.
  - `:pretty <expr>` — Show pretty-printed version of parsed expression.
- Basic arithmetic primitives: `+`, `-`, `*`, `=`, `&&`, `||`.
- Parser implemented using `parsec`.
- Small-step evaluator for `:trace`.
- `pretty :: Expr -> String` for converting AST to readable format.
- Demo runner to showcase all features automatically.

---

## [Unreleased]

- Plans for Stage 3: Add type checker, advanced error reporting, module system, optimizations, benchmark suite.
