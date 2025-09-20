module Main where

import Test.Tasty

import qualified Tests.Unit.Core as UnitCore
import qualified Tests.Unit.FileDiscovery as UnitFileDiscovery
import qualified Tests.Property.Core as PropertyCore

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Lintomatic Tests"
  [ unitTests
  , propertyTests
  ]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ UnitCore.tests
  , UnitFileDiscovery.tests
  ]

propertyTests :: TestTree
propertyTests = testGroup "Property-Based Tests"
  [ PropertyCore.tests
  ]