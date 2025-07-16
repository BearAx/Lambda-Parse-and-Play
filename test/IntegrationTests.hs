module IntegrationTests where

import Test.Hspec
import System.Directory
import System.FilePath
import System.Process (readProcess)
import Control.Exception (try, SomeException)

integrationTests :: Spec
integrationTests = describe "Integration Tests" $ do
  it "runs all examples without errors" $ do
    files <- listDirectory "examples"
    let lamFiles = filter (".lam" `isSuffixOf`) files
    results <- mapM runExample lamFiles
    all (== True) results `shouldBe` True

runExample :: FilePath -> IO Bool
runExample file = do
  let path = "examples" </> file
  result <- try (readProcess "stack" ["exec", "lambda-exe", path] "") :: IO (Either SomeException String)
  case result of
    Left _ -> return False
    Right output -> do
      putStrLn $ "✓ " ++ file ++ " passed"
      return True
