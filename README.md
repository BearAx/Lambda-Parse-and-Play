
# λ Parse & Play
*A Minimal Lambda-Calculus Parser with Interactive REPL*  

Semester — **[Sum25] Programming in Haskell**  
Authors  |  Aleksandr Medvedev · Nikita Shankin  

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

The entire Stage I fits in **~260 lines** of plain Haskell (no TH, no
monad transformers, only `parsec` + `containers`).

---

## Key ideas / Why it matters

| 💡 Idea | Why it matters |
|---------|-----------------|
| **Symmetric parser ↔ pretty-printer** | <br>⇢ любой вывод REPL можно скормить обратно без изменений; помогает отладке и автотестам |
| **Currying via `VPrim` + `curry2`** | примитивы (`+`, `&&`) работают как обычные λ-функции: `(+ 3) 5` OK |
| **Closures with lexical env** | чистый CBV без глобального состояния |
| **Single-file MVP** | легко читать и демонстрировать на защите |

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
| QuickCheck round-trip test | **optional** | property included in `test/Spec.hs` |

---

## Road-map (Stage II & beyond)

| 🚀 Planned feature | Adds |
|-------------------|------|
| `let` / `letrec`  | syntactic sugar, easy desugaring to λ |
| ASCII β-reduction trace | step-by-step animation in REPL |
| Module system / file loader | run larger examples |
| Benchmarks vs naïve evaluator | showcase optimisations (CSE / inlining) |

---

## Getting started

```bash
# clone & enter
git clone https://github.com/<your-org>/lambda-parse-play.git
cd lambda-parse-play

# build (Stack)
stack build          # downloads GHC & deps (parsec, containers)
stack run            # launches REPL

# or with Cabal
cabal update
cabal build
cabal run lambda-calc
