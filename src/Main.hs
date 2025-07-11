{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Minimal Lambda-Calculus interpreter — Stage I (ASCII-only console)
-- + symmetric pretty-printer (parse . pretty  ≡  Right).
---------------------------------------------------------------------
module Main (main) where

import           Control.Monad      (void)
import           Data.Char          (isAlphaNum, isLower, isSpace)
import qualified Data.Map.Strict    as M
import           System.IO          (hSetEncoding, stdout, utf8)
import           Text.Parsec        hiding (token)
import           Text.Parsec.String (Parser)

------------------------------------------------------------
-- 1. Abstract syntax tree
------------------------------------------------------------
data Literal = LInt Int | LBool Bool deriving (Eq)
instance Show Literal where
  show (LInt n)  = show n
  show (LBool b) = if b then "true" else "false"

data Expr
  = Var String
  | Lam String Expr
  | App Expr Expr
  | Lit Literal
  | Prim String
  deriving (Eq, Show)

------------------------------------------------------------
-- 2. Runtime values
------------------------------------------------------------
type Env = M.Map String Value
data Value
  = VLit Literal
  | VClos String Expr Env
  | VPrim (Value -> Either String Value)
instance Show Value where
  show = \case
    VLit l  -> show l
    VClos{} -> "<closure>"
    VPrim{} -> "<prim>"

------------------------------------------------------------
-- 3. Built-in primitive environment
------------------------------------------------------------
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
  -- | Lift a binary function on Literals to a curried runtime Value.
  curry2 :: (Literal -> Literal -> Either String Value) -> Value
  curry2 g = VPrim $ \v1 -> Right $ VPrim $ \v2 -> case (v1, v2) of
                  (VLit l1, VLit l2) -> g l1 l2
                  _                  -> Left "arguments must be literals"

  binInt op = curry2 $ \l1 l2 -> case (l1, l2) of
                 (LInt a, LInt b) -> Right (VLit (LInt (op a b)))
                 _                -> Left "type error: expected Int"

  binIntCmp cmp = curry2 $ \l1 l2 -> case (l1, l2) of
                     (LInt a, LInt b) -> Right (VLit (LBool (cmp a b)))
                     _                -> Left "type error: expected Int"

  binBool op = curry2 $ \l1 l2 -> case (l1, l2) of
                 (LBool a, LBool b) -> Right (VLit (LBool (op a b)))
                 _                  -> Left "type error: expected Bool"

emptyEnv :: Env
emptyEnv = M.fromList primEnv

------------------------------------------------------------
-- 4. Evaluation (call-by-value)
------------------------------------------------------------
eval :: Env -> Expr -> Either String Value
eval env = \case
  Var x       -> maybe (Left $ "unbound variable " ++ x) Right (M.lookup x env)
  Lit l       -> Right (VLit l)
  Prim p      -> maybe (Left $ "unknown primitive " ++ p) Right (lookup p primEnv)
  Lam x body  -> Right (VClos x body env)
  App e1 e2   -> do
    f <- eval env e1
    a <- eval env e2
    apply f a

apply :: Value -> Value -> Either String Value
apply (VClos x body clo) v = eval (M.insert x v clo) body
apply (VPrim f)           v = f v
apply _ _                  = Left "apply: non-function"

------------------------------------------------------------
-- 5. Parser (Parsec)
------------------------------------------------------------
ws :: Parser ()
ws = void $ many $ satisfy isSpace

ident :: Parser String
ident = do
  first <- satisfy (\c -> isLower c || c == '_')
  rest  <- many (satisfy isAlphaNum <|> oneOf "+*=/&|-_")
  ws
  pure (first:rest)

symbol :: String -> Parser String
symbol s = string s <* ws

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

lambdaP :: Parser Expr
lambdaP = do
  void (char '\\') *> ws
  args <- many1 ident
  void (char '.') *> ws
  body <- exprP
  pure $ foldr Lam body args

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
    identChar = satisfy isAlphaNum <|> oneOf "+*=/&|-_"
    boolLit s val = Lit (LBool val) <$ try (string s <* notFollowedBy identChar) <* ws
    intLit = do
      sign <- optionMaybe (char '-')
      digits <- many1 digit
      ws
      let n = read digits
      pure $ case sign of
        Nothing -> n
        Just _  -> -n
    primTok = choice $ map mkTok ["+","-","*","=","&&","||"]
    mkTok s = try (string s <* notFollowedBy identChar) <* ws *> pure s

aTermP :: Parser Expr
aTermP = atomP <|> lambdaP

exprP :: Parser Expr
exprP = foldl1 App <$> many1 aTermP

parseExpr :: String -> Either String Expr
parseExpr src = either (Left . show) Right (parse (ws *> exprP <* eof) "<stdin>" src)

------------------------------------------------------------
-- 6. Pretty-printer  (parse . pretty ≡ Right)
------------------------------------------------------------
pretty :: Expr -> String
pretty = go (0 :: Int)
  where
    par ctx me s | me < ctx  = "(" ++ s ++ ")"
                 | otherwise = s

    go _ (Var v)        = v
    go _ (Prim p)       = p
    go _ (Lit (LInt n)) = show n
    go _ (Lit (LBool b))= if b then "true" else "false"

    go ctx (Lam x body) =
      par ctx 1 $ "\\" ++ x ++ ". " ++ go 1 body

    go ctx app@App{}    =
      par ctx 2 $ unwords (map (go 2) (flatten app))
      where
        flatten (App f x) = flatten f ++ [x]
        flatten t         = [t]

------------------------------------------------------------
-- 7. REPL
------------------------------------------------------------
repl :: Env -> IO ()
repl env = do
  putStr ">> "
  line <- getLine
  case line of
    ":quit" -> putStrLn "Bye!"
    ":env"  -> mapM_ (\(k,v) -> putStrLn $ k ++ " = " ++ show v) (M.toList env) >> repl env
    _        -> case parseExpr line >>= eval env of
                  Left e            -> putStrLn ("Error: " ++ e) >> repl env
                  Right (VLit l)    -> putStrLn (pretty (Lit l)) >> repl env
                  Right v           -> print v >> repl env

------------------------------------------------------------
-- 8. Demo program
------------------------------------------------------------
sample :: String
sample = "(+ 4 5)"  -- expects 9

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
