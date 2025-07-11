{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

-- Minimal Lambda-Calculus interpreter — Stage I with symmetric pretty-printer
-- parse . pretty  ≡  Right, call-by-value semantics, ASCII REPL
module Main (main) where

import           Control.Monad      (void)                 -- for discarding parser results
import           Data.Char          (isAlphaNum, isLower, isSpace)
import qualified Data.Map.Strict    as M                    -- efficient map for variable environment
import           System.IO          (hSetEncoding, stdout, utf8)
import           Text.Parsec        hiding (token)          -- Parsec combinators for parsing
import           Text.Parsec.String (Parser)

-- Abstract syntax tree
--------------------------------------------------------------------------------
-- Defines the shape of our language:
--  * Literal: integer or boolean
--  * Expr: variables, lambdas, applications, literals, primitives
--------------------------------------------------------------------------------
data Literal
  = LInt  Int           -- integer literal, e.g. 42
  | LBool Bool          -- boolean literal, true/false
  deriving (Eq)

instance Show Literal where
  show (LInt n)  = show n
  show (LBool b) = if b then "true" else "false"

data Expr
  = Var String         -- variable reference
  | Lam String Expr    -- lambda abstraction: \x. expr
  | App Expr Expr      -- function application: f x
  | Lit Literal        -- literal value
  | Prim String        -- primitive operator, e.g. "+"
  deriving (Eq, Show)

-- Runtime values
--------------------------------------------------------------------------------
-- Defines how expressions are represented at runtime:
--  * VLit: literal values
--  * VClos: closures capturing lambda bodies with their environment
--  * VPrim: host-language functions for primitives, curried
--------------------------------------------------------------------------------
type Env = M.Map String Value

data Value
  = VLit  Literal                           -- literal wrapped as a value
  | VClos String Expr Env                   -- closure: parameter, body, and captured Env
  | VPrim (Value -> Either String Value)    -- curried primitive operation
instance Show Value where
  show = \case
    VLit l  -> show l
    VClos{} -> "<closure>"
    VPrim{} -> "<prim>"

-- Built-in primitive environment
--------------------------------------------------------------------------------
-- Lifts Haskell integer/boolean operations into our Value type:
--  * curry2: creates a curried function of two literal args
--  * binInt, binIntCmp, binBool: type-check and wrap op results
--------------------------------------------------------------------------------
primEnv :: [(String, Value)]
primEnv =
  [ ("+",  binInt  (+))
  , ("-",  binInt  (-))
  , ("*",  binInt  (*))
  , ("=",  binIntCmp (==))
  , ("&&", binBool (&&))
  , ("||", binBool (||))
  ]
 where
  curry2 :: (Literal -> Literal -> Either String Value) -> Value
  curry2 f = VPrim $ \v1 -> Right $ VPrim $ \v2 ->
    case (v1,v2) of
      (VLit l1, VLit l2) -> f l1 l2
      _                  -> Left "arguments must be literals"

  binInt op = curry2 $ \l1 l2 -> case (l1,l2) of
    (LInt a, LInt b) -> Right . VLit . LInt $ op a b
    _                -> Left "type error: expected Int"

  binIntCmp cmp = curry2 $ \l1 l2 -> case (l1,l2) of
    (LInt a, LInt b) -> Right . VLit . LBool $ cmp a b
    _                -> Left "type error: expected Int"

  binBool op = curry2 $ \l1 l2 -> case (l1,l2) of
    (LBool a, LBool b) -> Right . VLit . LBool $ op a b
    _                  -> Left "type error: expected Bool"

emptyEnv :: Env
emptyEnv = M.fromList primEnv  -- initial environment with only primitives

-- Evaluation (call-by-value)
--------------------------------------------------------------------------------
-- Implements CBV semantics:
--  * eval: recursively evaluates expressions in an Env
--  * apply: applies closures or primitives to evaluated arguments
--------------------------------------------------------------------------------
eval :: Env -> Expr -> Either String Value
eval env = \case
  Var x      -> maybe (Left $ "unbound variable " ++ x) Right (M.lookup x env)
  Lit l      -> Right $ VLit l
  Prim p     -> maybe (Left $ "unknown primitive " ++ p) Right (lookup p primEnv)
  Lam x b    -> Right $ VClos x b env
  App e1 e2  -> do
    vf <- eval env e1      -- evaluate function expression
    va <- eval env e2      -- evaluate argument expression
    apply vf va            -- apply result to arg

apply :: Value -> Value -> Either String Value
apply (VClos x body clo) v = eval (M.insert x v clo) body -- closure application
apply (VPrim f)       v     = f v                         -- primitive application
apply _ _                  = Left "apply: non-function"   -- error if not applicable

-- Parser (Parsec)
--------------------------------------------------------------------------------
-- Combinator-based parser that builds Expr AST:
--  * ws: skip whitespace
--  * ident/symbol/parens: basic lexing helpers
--  * lambdaP: parses \args. body → nested Lam
--  * atomP: literals, primitives, variables, or parenthesized expr
--  * exprP: left-associative application via many1 + foldl1
--------------------------------------------------------------------------------
ws :: Parser ()
ws = void $ many $ satisfy isSpace  -- consume spaces, tabs, newlines

ident :: Parser String
ident = do
  c  <- satisfy (\ch -> isLower ch || ch == '_')     -- start with lowercase or _
  cs <- many (satisfy isAlphaNum <|> oneOf "+*=/&|-_")
  ws
  pure (c:cs)

symbol :: String -> Parser String
symbol s = string s <* ws  -- parse fixed token and skip trailing ws

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"  -- handle ( ... )

lambdaP :: Parser Expr
lambdaP = do
  void (char '\\') *> ws
  args <- many1 ident
  void (char '.') *> ws
  body <- exprP
  pure $ foldr Lam body args  -- build nested Lam from arg list

atomP :: Parser Expr
atomP = choice
  [ Lit . LInt  <$> try intLit
  , boolLit "true"  True
  , boolLit "false" False
  , Prim <$> primTok
  , Var  <$> ident
  , parens exprP
  ]
 where
  identChar  = satisfy isAlphaNum <|> oneOf "+*=/&|-_"
  boolLit s v = Lit (LBool v) <$ try (string s <* notFollowedBy identChar) <* ws
  intLit = do
    sign <- optionMaybe (char '-')
    ds   <- many1 digit
    ws
    let n = read ds
    pure $ maybe n (const (-n)) sign
  primTok = choice (map mkTok ["+","-","*","=","&&","||"])
  mkTok s = try (string s <* notFollowedBy identChar) <* ws *> pure s

aTermP :: Parser Expr
aTermP = atomP <|> lambdaP

exprP :: Parser Expr
exprP = foldl1 App <$> many1 aTermP  -- combine atoms into left-assoc App

parseExpr :: String -> Either String Expr
parseExpr src = either (Left . show) Right $
  parse (ws *> exprP <* eof) "<stdin>" src

-- Pretty-printer (AST → String)
--------------------------------------------------------------------------------
-- Mirrors parser syntax so that `parse . pretty ≡ Right`:
--  * handles Var, Prim, Lit, Lam, App with correct parentheses based on precedence
--------------------------------------------------------------------------------
pretty :: Expr -> String
pretty = go (0 :: Int)
 where
  par ctx pri s | pri < ctx  = "(" ++ s ++ ")" | otherwise = s

  go _   (Var v)         = v
  go _   (Prim p)        = p
  go _   (Lit (LInt n))  = show n
  go _   (Lit (LBool b)) = if b then "true" else "false"

  go ctx (Lam x b) = par ctx 1 $
    "\\" ++ x ++ ". " ++ go 1 b

  go ctx app@App{} = par ctx 2 $
    unwords (map (go 2) (flatten app))
   where
    flatten (App f x) = flatten f ++ [x]
    flatten t         = [t]

-- REPL (Read-Eval-Print Loop)
--------------------------------------------------------------------------------
-- Simple console loop:
--  * ":quit" to exit
--  * ":env" to display current environment bindings
--  * otherwise parse, eval, and pretty-print results
--------------------------------------------------------------------------------
repl :: Env -> IO ()
repl env = do
  putStr ">> "
  line <- getLine
  case line of
    ":quit" -> putStrLn "Bye!"
    ":env"  -> mapM_ (\(k,v) -> putStrLn $ k ++ " = " ++ show v) (M.toList env)
               >> repl env
    _       -> case parseExpr line >>= eval env of
                 Left err       -> putStrLn ("Error: " ++ err) >> repl env
                 Right (VLit l) -> putStrLn (pretty (Lit l)) >> repl env
                 Right v        -> print v >> repl env

-- Main / Demo
--------------------------------------------------------------------------------
-- Sets UTF-8, runs a sample expression, then enters REPL
--------------------------------------------------------------------------------
sample :: String
sample = "(+ 4 5)"  -- expected result: 9

main :: IO ()
main = do
  hSetEncoding stdout utf8
  putStrLn "Lambda-Calc REPL  (:quit to exit)"
  putStrLn $ "Sample: " ++ sample ++ " =>"
  case parseExpr sample >>= eval emptyEnv of
    Left err       -> putStrLn ("Sample error: " ++ err)
    Right (VLit l) -> putStrLn (pretty (Lit l))
    Right v        -> print v
  repl emptyEnv
