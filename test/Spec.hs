module Main where

import Test.Hspec
import UnitTests
import IntegrationTests
import PerformanceTests

main :: IO ()
main = hspec $ do
  unitTests
  integrationTests
  performanceTests
