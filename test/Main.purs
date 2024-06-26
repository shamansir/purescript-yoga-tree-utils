module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Yoga.Tree (showTree)
import Yoga.Tree.Extended (Tree(..))
import Yoga.Tree.Extended (node, leaf) as Tree


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "purescript-grammar" $ do
    it "foo" $ do
      "foo" `shouldEqual` "foo"