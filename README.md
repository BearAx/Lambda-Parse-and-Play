
# λ Parse & Play
*A Lambda-Calculus Playground: CBV Evaluation, Let Bindings & REPL*

**[Sum25] Programming in Haskell**  
## Authors
  <tr>
    <td align="center">
      <a href="https://github.com/BearAx">
        <img src="https://github.com/BearAx.png" width="80" height="80" style="border-radius: 50%;" /><br />
        <b>Aleksandr Medvedev</b>
      </a>
    </td>
    <td align="center">
      <a href="https://github.com/Mysteri0K1ng">
        <img src="https://github.com/Mysteri0K1ng.png" width="80" height="80" style="border-radius: 50%;" /><br />
        <b>Nikita Shankin</b>
      </a>
    </td>
  </tr>

---

## Table of Contents
- [What is it?](#what-is-it)
- [Key ideas / Why it matters](#key-ideas--why-it-matters)
- [What is done](#what-is-done-)
- [Road-map](#road-map)
- [Getting started](#getting-started)
- [CHANGELOG.md](CHANGELOG.md) -- not configured yet
- [LICENSE](LICENSE)

---

## What is it?

`λ Parse & Play` is a toy-yet-practical implementation of the **untyped
lambda calculus** enriched with

* extended syntax:
  * Integer (`52`) and boolean literals (`true`, `false`)
  * Primitive operations: `+ - * = && ||`
  * Syntactic sugar: `let/in` bindings, `if/then/else` 
* Core features :   
  * Parsec-based parser  with round-trip property (`parseExpr . pretty ≡ Right`)  
  * Call-by-value evaluator  with closures and primitive operations
  * Interactive REPL  supporting commands: `:quit`, `:trace`, `:pretty`, `:load`
  * Step-by-step β-reduction tracing  for debugging evaluation
* Minimal footprint :
  * Entire implementation fits in ~300 lines  of clean Haskell (uses only `parsec` and `containers`)
    
---

## Key ideas / Why it matters

| 💡 Idea | Why it matters |
|---------|-----------------|
| **Symmetric parser ↔ pretty-printer** | <br>⇢ Ensures `parseExpr . pretty ≡ Right` - any REPL output can be re-parsed without modification, enabling reliable testing and debugging. |
| **Currying via `VPrim` + `curry2`** | primitives (`+`, `&&`) work like ordinary λ-functions: `(+ 3) 5` OK |
| **Lexical closures with environment** | Functions capture their definition scope (`VClos`), enabling proper scoping and recursion. |
| **Step-by-step β-trace** |`:trace` visualizes evaluation steps (β-reduction, δ-reduction), ideal for learning lambda calculus mechanics. |
| **Parser combinators (Parsec)** | Modular grammar construction (e.g., `ident`, `lambdaP`, `exprP`) avoids parser generators (Happy). |
| **Left-associative application** | `foldl1 App <$> many1 atomP` implements left-associative function application in one line. |
| **Error handling with diagnostics** | Parsec tracks positions (`line/column`) for precise errors (e.g., `unexpected "+" expecting "(")`. |
| **Backtracking with `try`** | Safe parsing of reserved words (`true`, `let`) using `try (string s <* notFollowedByIdChar)`. |
| **Type-safe AST** | Haskell’s type system guarantees valid AST construction (e.g., `parseExpr` returns `Expr` on success). |
| **REPL interactivity** | Commands like `:trace`, `:pretty`, `:load` turn it into a hands-on playground for lambda calculus. |
| **Substitution-based β-reduction** | `subst` safely replaces variables in lambda bodies, avoiding variable capture. |
| **Primitive δ-reduction** | Built-in ops (e.g., `(+ 3 4) → 7`) integrate seamlessly with λ-calculus evaluation. |
| **Minimal footprint** | Entire interpreter fits in ~300 lines of idiomatic Haskell (only `parsec` + `containers` dependencies). |

---

## What is done ✅

| Component | Status | Notes |
|-----------|--------|-------|
| AST | **✔** | (`Var/Lam/App/Lit/Prim/let/if`) |
| Parsec parser | **✔** | left-assoc application, literals, λ |
| `let..in`  | **✔** | Supports variable scoping: `let x = e1 in e2` |
| Conditionals | **✔** | `if c then t else f` with boolean guards |
| Pretty-printer | **✔** | See clear output with `:pretty` command |
| ASCII β-reduction trace | **✔** | Step-by-step reduction with `:trace` command |
| Module system / file loader |  **✔** | run larger examples with `:load` command |
| REPL commands |  **✔** | `:quit`, `:env`, `:load`, `:trace`, `:pretty`, error handling |
| Sample demo |  **✔** | demo tests all interpreter features |
| `-Wall` clean build | **✔** | no warnings after default-type fix |

---

## Road-map

| 🚀 Planned feature | Adds |
|-------------------|------|
| Recursive `letrec`  | Support self-referential bindings (e.g., factorial) |
| Module system | Import/Export definitions across files |
| Optimizations | Common subexpression elimination, let-floating |
| Benchmark suite | Compare performance vs naive evaluators |

---

## Getting started

```bash
# clone & enter
git clone https://github.com/BearAx/Lambda-Parse-and-Play.git
cd Lambda-Parse-and-Play

# build (Stack)
stack build          # downloads GHC & deps (parsec, containers)
stack run            # launches REPL

# or with Cabal
cabal update
cabal build
cabal run lambda-calc
