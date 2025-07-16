{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

-- Entry point for the Lambda-Calc-and-Play with support for:
-- * Lambda calculus with integers, booleans, and primitives
-- * `let/in` bindings
-- * Pretty-printer (parse . pretty ≡ Right)
-- * REPL with commands :quit, :load (..-trace, ..-pretty), :trace, :pretty

module Main (main) where

import           System.IO              (hFlush, hSetEncoding, stdout, utf8)
import qualified Data.Map.Strict        as M
import           Data.Char              (isAlphaNum, isLower, isSpace)
import           Data.List              (isPrefixOf)
import           Control.Applicative    (many, (<|>))

-- Parsec imports for parser implementation
import           Text.Parsec            (parse, try, eof, skipMany)
import           Text.Parsec.String     (Parser)
import           Text.Parsec.Char       (digit, string, satisfy)
import           Text.Parsec.Combinator (between, choice, many1)

------------------------------------------------------------
-- 1. Abstract Syntax Tree (AST)
-- Defines all the expression types supported in the language.
------------------------------------------------------------
data Literal = LInt Int | LBool Bool deriving (Eq)

-- Custom Show instance to display literals as "true"/"false"
instance Show Literal where
  show (LInt n)  = show n
  show (LBool b) = if b then "true" else "false"

-- Core expression types: variables, lambdas, applications, literals, primitives, let-bindings
data Expr
  = Var  String
  | Lam  String Expr
  | App  Expr Expr
  | Lit  Literal
  | Prim String
  | Let  String Expr Expr
  | If   Expr Expr Expr
  | LetRec String Expr Expr
  deriving (Eq, Show)

------------------------------------------------------------
-- 2. Runtime Values and Environment
-- Values during evaluation, such as closures and primitive functions.
------------------------------------------------------------
type Env = M.Map String Value

-- Value types: literals, closures (with env), and primitive functions
data Value
  = VLit  Literal
  | VClos String Expr Env
  | VPrim (Value -> Either String Value)

-- Custom Show instance for values
instance Show Value where
  show = \case
    VLit l  -> show l
    VClos{} -> "<closure>"
    VPrim{} -> "<prim>"

-- Application of closures or primitive operations
apply :: Value -> Value -> Either String Value
apply (VClos x b clo) v = eval (M.insert x v clo) b
apply (VPrim f)       v = f v
apply _ _               = Left "apply: non-function"

------------------------------------------------------------
-- 2a. Primitive Environment
-- Defines built-in operators like +, *, =, etc.
------------------------------------------------------------
primEnv :: [(String,Value)]
primEnv =
  [ ("+",  binInt  (+))
  , ("-",  binInt  (-))
  , ("*",  binInt  (*))
  , ("=",  binCmp  (==))
  , ("&&", binBool (&&))
  , ("||", binBool (||))
  ]
 where
  -- Helper: curries a binary literal operation
  curry2 g = VPrim $ \v1 -> Right $ VPrim $ \v2 ->
    case (v1,v2) of
      (VLit l1, VLit l2) -> g l1 l2
      _ -> Left "arguments must be literals"

  -- Integer and boolean binary ops
  binInt  op = curry2 $ \l1 l2 -> case (l1,l2) of
    (LInt a,LInt b) -> Right (VLit (LInt (op a b)))
    _ -> Left "Int expected"

  binCmp  c  = curry2 $ \l1 l2 -> case (l1,l2) of
    (LInt a,LInt b) -> Right (VLit (LBool (c a b)))
    _ -> Left "Int expected"

  binBool op = curry2 $ \l1 l2 -> case (l1,l2) of
    (LBool a,LBool b) -> Right (VLit (LBool (op a b)))
    _ -> Left "Bool expected"

-- Initial environment with only primitives
emptyEnv :: Env
emptyEnv = M.fromList primEnv

------------------------------------------------------------
-- 3. Evaluation (call-by-value + let)
-- Core interpreter logic: evaluates expressions in an environment.
------------------------------------------------------------
eval :: Env -> Expr -> Either String Value
eval env = \case
  Var x        -> maybe (Left $ "unbound variable " ++ x) Right (M.lookup x env)
  Lit l        -> Right (VLit l)
  Prim p       -> maybe (Left $ "unknown prim " ++ p)  Right (lookup p primEnv)
  Lam x b      -> Right (VClos x b env)
  App e1 e2    -> eval env e1 >>= \f  -> eval env e2 >>= apply f
  Let x e1 e2  -> eval env e1 >>= \v1 -> eval (M.insert x v1 env) e2
  If c t f     -> do
    cond <- eval env c
    case cond of
      VLit (LBool True)  -> eval env t
      VLit (LBool False) -> eval env f
      _ -> Left "if condition must be a boolean"
  LetRec x e1 e2 ->
    let recEnv = M.insert x v env
        v = case eval recEnv e1 of
              Right val -> val
              Left err  -> error ("letrec error: " ++ err)
    in eval recEnv e2



------------------------------------------------------------
-- 4. Parser (using Parsec)
-- Converts string input into Expr AST.
------------------------------------------------------------
-- Whitespace and lexing helpers
ws       :: Parser (); ws = skipMany (satisfy isSpace)
lexeme   :: Parser a -> Parser a;  lexeme p    = p <* ws
symbol   :: String   -> Parser String; symbol  = lexeme . try . string

-- Reserved keywords that can't be used as identifiers
reservedWords :: [String]; reservedWords = ["let", "letrec", "in", "true", "false", "if", "then", "else"]

-- Parses a reserved word and ensures it's not used as an identifier
reserved :: String -> Parser String
reserved w = try (string w <* notFollowedByIdChar) <* ws
 where
   notFollowedByIdChar =
     (Just <$> satisfy (\c -> isAlphaNum c || c == '_')) <|> pure Nothing
     >>= maybe (return ()) (const (fail ""))

-- Parses identifiers (variable names)
ident :: Parser String
ident = lexeme . try $ do
  c  <- satisfy (\ch -> isLower ch || ch == '_')
  cs <- many (satisfy (\ch -> isAlphaNum ch || ch `elem` "+*=/&|-_"))
  let name = c:cs
  if name `elem` reservedWords
     then fail $ "reserved word " ++ show name ++ " cannot be identifier"
     else return name

-- Parsers for literals, primitives, and language constructs
intLit   :: Parser Expr
intLit   = Lit . LInt . read <$> lexeme (many1 digit)

boolLit  :: Parser Expr
boolLit  =  Lit (LBool True)  <$ reserved "true"
        <|> Lit (LBool False) <$ reserved "false"

primP    :: Parser Expr
primP    = Prim <$> lexeme (choice (map (try . symbol)
          ["+","-","*","=","&&","||"]))

lambdaP  :: Parser Expr
lambdaP  = do
  _ <- symbol "\\"
  v <- ident
  _ <- symbol "."
  Lam v <$> exprP

letP     :: Parser Expr
letP     = do
  _  <- reserved "let"
  x  <- ident
  _  <- symbol "="
  e1 <- exprP
  _  <- reserved "in"
  Let x e1 <$> exprP

ifP :: Parser Expr
ifP = do
  _ <- reserved "if"
  c <- exprP
  _ <- reserved "then"
  t <- exprP
  _ <- reserved "else"
  f <- exprP
  pure (If c t f)

letrecP :: Parser Expr
letrecP = do
  _  <- reserved "letrec"
  x  <- ident
  _  <- symbol "="
  e1 <- exprP
  _  <- reserved "in"
  e2 <- exprP
  pure (LetRec x e1 e2)

-- Parses atomic expressions and parenthesized sub-expressions
atomP :: Parser Expr
atomP = choice
  [ letrecP
  , ifP, letP, lambdaP, intLit, boolLit, primP
  , Var <$> ident
  , between (symbol "(") (symbol ")") exprP
  ]

-- Parses full expressions by left-associative application
exprP :: Parser Expr
exprP = foldl1 App <$> many1 atomP

-- Runs the parser on a string
parseExpr :: String -> Either String Expr
parseExpr s = either (Left . show) Right (parse (ws *> exprP <* eof) "<stdin>" s)

------------------------------------------------------------
-- 5. Pretty-printer
-- Converts an Expr back into readable source-like string.
------------------------------------------------------------
pretty :: Expr -> String
pretty e = case e of
  Var v         -> v
  Prim p        -> p
  Lit (LInt n)  -> show n
  Lit (LBool b) -> if b then "true" else "false"
  Lam x b       -> "\\" ++ x ++ ". " ++ pretty b
  Let x a b     -> "let " ++ x ++ " = " ++ pretty a ++ " in " ++ pretty b
  If c t f      -> "if " ++ pretty c ++ " then " ++ pretty t ++ " else " ++ pretty f
  LetRec x a b  -> "letrec " ++ x ++ " = " ++ pretty a ++ " in " ++ pretty b
  _             -> "(" ++ unwords (map pretty (flatten e)) ++ ")"
 where
  flatten (App f a) = flatten f ++ [a]
  flatten t         = [t]

------------------------------------------------------------
-- 6. β-trace (for :trace)
-- Small-step evaluator with Δ and β rules.
------------------------------------------------------------
-- Substitution function for β-reduction
subst :: String -> Expr -> Expr -> Expr
subst x r = go where
  go (Var y)       | y == x    = r
                   | otherwise = Var y
  go (Lam y b)     | y == x    = Lam y b
                   | otherwise = Lam y (go b)
  go (App f a)                 = App (go f) (go a)
  go (Let y e1 e2) | y == x    = Let y (go e1) e2
                   | otherwise = Let y (go e1) (go e2)
  go (LetRec y e1 e2) | y == x    = LetRec y (go e1) e2
                      | otherwise = LetRec y (go e1) (go e2)
  go (Lit l)                   = Lit l
  go (Prim p)                  = Prim p
  go (If c t f)                = If (go c) (go t) (go f)


-- Delta reduction: applies built-in primitives to literal arguments
delta :: Expr -> Maybe Expr
delta (App (App (Prim p) (Lit l1)) (Lit l2)) =
  case (p, l1, l2) of
    ("+",  LInt a,  LInt b)  -> Just (Lit (LInt  (a + b)))
    ("-",  LInt a,  LInt b)  -> Just (Lit (LInt  (a - b)))
    ("*",  LInt a,  LInt b)  -> Just (Lit (LInt  (a * b)))
    ("=",  LInt a,  LInt b)  -> Just (Lit (LBool (a == b)))
    ("&&", LBool a, LBool b) -> Just (Lit (LBool (a && b)))
    ("||", LBool a, LBool b) -> Just (Lit (LBool (a || b)))
    _                        -> Nothing
delta _ = Nothing

-- Small-step evaluation (1-step beta/delta reduction)
step :: Expr -> Maybe Expr
step e | Just e' <- delta e          = Just e'                      -- delta step
step (App (Lam x b) a)              = Just (subst x a b)           -- beta step
step (App f a)                      = App <$> step f <*> pure a
                                   <|> App f     <$> step a
step (Lam x b)                      = Lam x <$> step b
step (Let x e1 e2)                  = Let x <$> step e1 <*> pure e2
                                   <|> Just (subst x e1 e2)
step (If c t f) = If <$> step c <*> pure t <*> pure f
               <|> case c of
                    Lit (LBool True)  -> Just t
                    Lit (LBool False) -> Just f
                    _                 -> Nothing
step (LetRec x e1 e2) = Just (subst x (LetRec x e1 e1) e2)
step _                              = Nothing

-- Traces each evaluation step until expression cannot be reduced further
traceExpr :: Expr -> IO ()
traceExpr = traceExprWithLimit 500

-- Same but with step limit (to detect infinite loops)
traceExprWithLimit :: Int -> Expr -> IO ()
traceExprWithLimit 0 _ = putStrLn "-- stopped: exceeded 500 steps (possible infinite loop)"
traceExprWithLimit n e = do
  putStrLn (pretty e)
  case step e of
    Just e' -> traceExprWithLimit (n - 1) e'
    Nothing -> putStrLn "-- done --"


------------------------------------------------------------------------------------------
-- 7. REPL
-- Interactive command-line loop with :trace, :pretty, :load (..-trace, ..-pretty)
------------------------------------------------------------------------------------------
repl :: Env -> IO ()
repl env = do
  putStr ">> " >> hFlush stdout
  ln <- getLine
  case () of
    _ | ln == ":quit"                   -> putStrLn "Bye!"
      | ":load " `isPrefixOf` ln        -> readFile (drop 6 ln) >>= run
      | ":load-trace " `isPrefixOf` ln  -> readFile (drop 12 ln) >>= runTrace
      | ":load-pretty " `isPrefixOf` ln -> readFile (drop 13 ln) >>= runPretty
      | ":trace "  `isPrefixOf` ln      -> runTrace (drop 7 ln)
      | ":pretty " `isPrefixOf` ln      -> runPretty (drop 8 ln)
      | otherwise                       -> run ln
 where
  -- Normal execution path
  run src = case parseExpr src >>= eval env of
    Left err       -> putStrLn ("Error: " ++ err) >> repl env
    Right (VLit l) -> putStrLn (pretty (Lit l))   >> repl env
    Right v        -> print v                     >> repl env

  -- Tracing intermediate evaluation steps
  runTrace src = case parseExpr src of
    Left err -> putStrLn ("Parse error: " ++ err) >> repl env
    Right e  -> traceExpr e                       >> repl env

  -- Pretty-printing without evaluation
  runPretty src = case parseExpr src of
    Left err -> putStrLn ("Parse error: " ++ err) >> repl env
    Right e  -> putStrLn (pretty e)               >> repl env

------------------------------------------------------------
-- 8. Demo & main
-- Executes sample expression and starts REPL
------------------------------------------------------------
demo :: IO ()
demo = do
  putStrLn "-- DEMO: evaluate let/lambda/arithmetic expression"
  putStrLn ">> let dbl = \\x. (* x 2) in dbl (+ 3 4)"
  printResult "let dbl = \\x. (* x 2) in dbl (+ 3 4)"

  putStrLn "\n-- DEMO: trace evaluation (beta and lambda steps)"
  putStrLn ">> :trace let dbl = \\x. (+ x x) in dbl (+ 1 2)"
  traceExprFrom "let dbl = \\x. (+ x x) in dbl (+ 1 2)"

  putStrLn "\n-- DEMO: letrec factorial"
  putStrLn ">> letrec fact = \\n. if (= n 0) then 1 else (* n (fact (- n 1))) in fact 5"
  printResult "letrec fact = \\n. if (= n 0) then 1 else (* n (fact (- n 1))) in fact 5"
  
  putStrLn "\n-- DEMO: pretty-printer"
  putStrLn ">> :pretty (\\x. (+ x x))"
  printPretty "\\x. (+ x x)"

  putStrLn "\n-- DEMO: parser error"
  printResult "let a = 5 inx 2"

  putStrLn "\n-- DEMO: unbound variable error"
  printResult "sum 5"

-- Executes normal evaluation
printResult :: String -> IO ()
printResult src = case parseExpr src >>= eval emptyEnv of
  Left err       -> putStrLn $ "Error: " ++ err
  Right (VLit l) -> putStrLn $ pretty (Lit l)
  Right v        -> print v

-- Executes trace
traceExprFrom :: String -> IO ()
traceExprFrom src = case parseExpr src of
  Left err -> putStrLn $ "Parse error: " ++ err
  Right e  -> traceExpr e

-- Executes pretty-print
printPretty :: String -> IO ()
printPretty src = case parseExpr src of
  Left err -> putStrLn $ "Parse error: " ++ err
  Right e  -> putStrLn $ pretty e

main :: IO ()
main = do
  hSetEncoding stdout utf8
  putStrLn "------------------------------------------------------------------------------"
  putStrLn "| Lambda-Parse-and-Play (:quit  :load <file>  :trace <expr>  :pretty <expr>) |"
  putStrLn "------------------------------------------------------------------------------\n"
  putStrLn "\n>>> Running demo of all features <<<\n"
  demo
  putStrLn "\n>>> Entering REPL:"
  repl emptyEnv
