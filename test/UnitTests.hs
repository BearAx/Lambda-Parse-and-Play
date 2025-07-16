module UnitTests where

import Test.Hspec
import AST
import Parser
import Eval

unitTests :: Spec
unitTests = describe "Unit Tests" $ do
  parserTests
  evalTests

parserTests :: Spec
parserTests = describe "Parser Tests" $ do
  it "parses simple variables" $
    parseExpr "x" `shouldBe` Right (Var "x")
  
  it "parses lambda abstractions" $
    parseExpr "\\x. x" `shouldBe` Right (Lam "x" (Var "x"))
  
  it "parses applications" $
    parseExpr "f a" `shouldBe` Right (App (Var "f") (Var "a"))
  
  it "parses nested expressions" $
    parseExpr "(\\x. x) (\\y. y)" `shouldBe` 
      Right (App (Lam "x" (Var "x")) (Lam "y" (Var "y")))
  
  it "handles complex expression" $
    parseExpr "\\x. \\y. x y (\\z. z)" `shouldBe`
      Right (Lam "x" (Lam "y" (App (App (Var "x") (Var "y")) (Lam "z" (Var "z")))))

evalTests :: Spec
evalTests = describe "Evaluation Tests" $ do
  it "evaluates identity function" $
    reduce (App (Lam "x" (Var "x")) (Var "y")) `shouldBe` Var "y"
  
  it "evaluates K combinator" $
    let k = Lam "x" (Lam "y" (Var "x"))
        expr = App (App k (Var "a")) (Var "b")
    in reduce expr `shouldBe` Var "a"
  
  it "evaluates S combinator" $
    let s = Lam "x" (Lam "y" (Lam "z" (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z")))))
        expr = App (App s (Lam "a" (Lam "b" (Var "a")))) (Lam "c" (Var "c")))
    in reduce expr `shouldBe` Lam "z" (Var "z")
  
  it "handles alpha conversion" $
    let expr = App (Lam "x" (Lam "x" (Var "x"))) (Var "y")
    in reduce expr `shouldBe` Lam "x'" (Var "x'")
  
  it "evaluates church numerals" $
    let two = Lam "f" (Lam "z" (App (Var "f") (App (Var "f") (Var "z"))))
        succ = Lam "n" (Lam "f" (Lam "z" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "z")))))
        expr = App succ two
    in reduce expr `shouldBe` 
          Lam "f" (Lam "z" (App (Var "f") (App (App (Lam "f" (Lam "z" (App (Var "f") (App (Var "f") (Var "z")))) 
          (Var "f")) (Var "z"))))
