module PerformanceTests where

import Test.Hspec
import AST
import Eval
import Data.Time.Clock

performanceTests :: Spec
performanceTests = describe "Performance Tests" $ do
  it "reduces large expression in under 1 second" $ do
    let expr = createLargeExpression 1000
    start <- getCurrentTime
    _ <- evaluate $ reduce expr
    end <- getCurrentTime
    let time = diffUTCTime end start
    putStrLn $ "Reduction time: " ++ show time
    time `shouldSatisfy` (< 1.0)

createLargeExpression :: Int -> Expr
createLargeExpression n = foldl1 compose (replicate n identity)
  where
    identity = Lam "x" (Var "x")
    compose f g = Lam "x" (App f (App g (Var "x")))
