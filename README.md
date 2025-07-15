
# λ Parse & Play
*A Minimal Lambda-Calculus Parser with Interactive REPL*  

**[Sum25] Programming in Haskell**  
## Authors 
<table>
  <tr>
    <td align="center">
      <a href="https://github.com/BearAx">
        <img src="https://github.com/BearAx.png" width="80" height="80" style="border-radius: 50%;" /><br />
        <sub><b>Aleksandr Medvedev</b></sub>
      </a>
    </td>
    <td align="center">
      <a href="https://github.com/Mysteri0K1ng">
        <img src="https://github.com/Mysteri0K1ng.png" width="80" height="80" style="border-radius: 50%;" /><br />
        <sub><b>Nikita Shankin</b></sub>
      </a>
    </td>
  </tr>
</table>

---

## Table of Contents
- [What is it?](#what-is-it)
- [Key ideas / Why it matters](#key-ideas--why-it-matters)
- [Stage I](#stage-i--done---)
- [Road-map](#road-map-stage-ii--beyond)
- [Getting started](#getting-started)
- [CHANGELOG.md](CHANGELOG.md) -- not configured yet
- [LICENSE](LICENSE)

---

## What is it?

`λ Parse & Play` is a toy-yet-practical implementation of the **untyped
lambda calculus** enriched with

* integer / boolean literals and six primitive operators  
  `+  -  *  =  &&  ||`
* a **Parsec** grammar that can *round-trip* with its pretty-printer  
  (`parseExpr . pretty ≡ Right`)
* a tiny **call-by-value interpreter** (closures + primitive ops)
* an ASCII **REPL** (`:quit`, `:env`) for live experimentation

The entire Stage I fits in **~240 lines** of plain Haskell (no TH, no
monad transformers, only `parsec` + `containers`).

---

## Key ideas / Why it matters

| 💡 Idea | Why it matters |
|---------|-----------------|
| **Symmetric parser ↔ pretty-printer** | <br>⇢ any REPL output can be fed back unchanged; it helps debugging and autotests |
| **Currying via `VPrim` + `curry2`** | primitives (`+`, `&&`) work like ordinary λ-functions: `(+ 3) 5` OK |
| **Closures with lexical env** | pure CBV with no global state |
| **Compositionality** | The parser is just `Parser A`. We assemble grammar like Lego from small blocks of `ident`, `lambdaP`, `exprP` |
| **Left-hand expression without LALR tables** | `foldl1 App <$> many1 aTermP` implements left associativity in one line; no parser generators are needed (Happy) |
| **Understandable mistakes** | Parsec stores the position (`line/column`), so if we make a typo, we `get unexpected "+" expecting"("` — easy to debug |
| **Backtracking with `try`** | For primitives/boolean words, we use `try (string s <* notFollowedBy ...)` — rollback if followed by a letter. It's safe and readable |
| **Type Safety** | The compiler guarantees that if `exprP :: Parser is Expr`, then the AST is exactly built upon success |
| **Less code** | 50 lines of Parsec vs. about 150-200 if written manually on `readP`/`State` |

---

## Stage I — done   ✅

| Component | Status | Notes |
|-----------|--------|-------|
| AST (`Var/Lam/App/Lit/Prim`) | **✔** | minimal core |
| Parsec parser | **✔** | left-assoc application, literals, λ |
| Pretty-printer | **✔** | round-trip property holds |
| CBV evaluator | **✔** | closures, primitive env |
| REPL + commands | **✔** | `:quit`, `:env`, error handling |
| Sample script | **✔** | `(+ 4 5)` prints `9` on start |
| `-Wall` clean build | **✔** | no warnings after default-type fix |

---

## Stage II — done   ✅

| Component | Status | Notes |
|-----------|--------|-------|
| AST extensions  | **✔** | Added `Let`, `If` nodes |
| `let..in`  | **✔** | Supports variable scoping: `let x = e1 in e2` |
| Conditionals | **✔** | `if c then t else f` with boolean guards |
| ASCII β-reduction trace | **✔** | Step-by-step reduction with `:trace` command |
| Module system / file loader |  **✔** | run larger examples with `:load` command |
| REPL commands |  **✔** | `:quit`, `:env`, `:load`, `:trace`, `:pretty`, error handling |

---

## Road-map (Stage III & beyond)

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
