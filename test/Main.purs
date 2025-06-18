module Test.Main where

import Prelude

import Data.String (toUpper) as String
import Data.Array (length) as Array
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))

import Effect (Effect)
import Effect.Aff (launchAff_)

import Control.Comonad.Cofree ((:<))
import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error) as Ex


import Test.Spec (describe, it, itOnly)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Yoga.Tree (showTree)
import Yoga.Tree.Extended (Tree, (:<~))
import Yoga.Tree.Extended (node, leaf, set, update, children, flatten, edges) as Tree
import Yoga.Tree.Extended.Path (Path(..))
import Yoga.Tree.Extended.Path (with, traverse, find, root, advance, up, toArray) as Path


ql = Tree.leaf


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "purescript-yoga-tree-utils" $ do

    describe "basics" $ do
      it "root is empty path" $ do
        Path.toArray Path.root `shouldEqual` []
      it "advancing works" $ do
        Path.toArray (Path.root # Path.advance 3 # Path.advance 3 # Path.advance 7)
        `shouldEqual`
        [ 3, 3, 7 ]
      it "going up works" $ do
        Path.toArray (Path.root # Path.advance 7 # Path.advance 4 # Path.advance 3 # Path.up)
        `shouldEqual`
        [ 7, 4 ]
      it "flatten works" $ do
        (Tree.flatten $ 'a' :< [ ql 'b', ql 'c', 'd' :<~ [ '1', '2', '3' ], ql 'e' ])
        `shouldEqual`
        [ 'a', 'b', 'c', 'd', '1', '2', '3', 'e' ]

    describe "`with`+`set`" $ do

      it "`with` : `set` on the root path" $
        (Path.with (Path []) (Tree.set 'b') $ ql 'a')
        `compareTrees`
        (ql 'b')
      it "`with` : `set` with one of the children" $
        (Path.with (Path [2]) (Tree.set 'x')
          $ 'a' :<~ [ 'b', 'c', 'd', 'e' ]
        )
        `compareTrees`
        ('a' :<~ [ 'b', 'c', 'x', 'e' ])
      it "`with` : `set` with the children that is an empty node" $
        (Path.with (Path [2]) (Tree.set 'x')
          $ 'a' :< [ ql 'b', ql 'c', 'd' :< [], ql 'e' ]
        )
        `compareTrees`
        ('a' :< [ ql 'b', ql 'c', 'x' :< [], ql 'e' ])
      it "`with` : `set` with the children that is a node with leafs" $
        (Path.with (Path [2]) (Tree.set 'x')
          $ 'a' :< [ ql 'b', ql 'c', 'd' :<~ [ '1', '2', '3' ], ql 'e' ]
        )
        `compareTrees`
        ('a' :< [ ql 'b', ql 'c', 'x' :<~ [ '1', '2', '3' ], ql 'e' ])
      it "`with` : `set` with the children on a deeper level" $
        (Path.with (Path [2, 0]) (Tree.set 'x')
          $ 'a' :< [ ql 'b', ql 'c', 'd' :<~ [ '1', '2', '3' ], ql 'e' ]
        )
        `compareTrees`
        ('a' :< [ ql 'b', ql 'c', 'd' :<~ [ 'x', '2', '3' ], ql 'e' ])
      it "`with` : `set` when child doesn't exist" $
        (Path.with (Path [2, 7]) (Tree.set 'x')
          $ 'a' :< [ ql 'b', ql 'c', 'd' :<~ [ '1', '2', '3' ], ql 'e' ]
        )
        `compareTrees`
        ('a' :< [ ql 'b', ql 'c', 'd' :<~ [ '1', '2', '3' ], ql 'e' ])

    describe "`with`+`update`" $ do

      it "`with` : `update` on the root path" $
        (Path.with (Path []) (Tree.update String.toUpper) $ ql "a")
        `compareTrees`
        (ql "A")
      it "`with` : `update` with one of the children" $
        (Path.with (Path [2]) (Tree.update String.toUpper)
          $ "a" :<~ [ "b", "c", "d", "e" ]
        )
        `compareTrees`
        ("a" :<~ [ "b", "c", "D", "e" ])
      it "`with` : `update` with the children that is an empty node" $ do
        (Path.with (Path [2]) (Tree.update String.toUpper)
          $ "a" :< [ ql "b", ql "c", "d" :< [], ql "e" ]
        )
        `compareTrees`
        ("a" :< [ ql "b", ql "c", "D" :< [], ql "e" ])
      it "`with` : `update` with the children that is a node with leafs" $
        (Path.with (Path [2]) (Tree.update String.toUpper)
          $ "a" :< [ ql "b", ql "c", "d" :<~ [ "q", "r", "s" ], ql "e" ]
        )
        `compareTrees`
        ("a" :< [ ql "b", ql "c", "D" :<~ [ "q", "r", "s" ], ql "e" ])
      it "`with` : `update` with the children on a deeper level" $
        (Path.with (Path [2, 0]) (Tree.update String.toUpper)
          $ "a" :< [ ql "b", ql "c", "d" :<~ [ "q", "r", "s" ], ql "e" ]
        )
        `compareTrees`
        ("a" :< [ ql "b", ql "c", "d" :<~ [ "Q", "r", "s" ], ql "e" ])
      it "`with` : `update` when child doesn't exist" $
        (Path.with (Path [2, 7]) (Tree.update String.toUpper)
          $ "a" :< [ ql "b", ql "c", "d" :<~ [ "q", "r", "s" ], ql "e" ]
        )
        `compareTrees`
        ("a" :< [ ql "b", ql "c", "d" :<~ [ "q", "r", "s" ], ql "e" ])

    describe "`traverse`" $ do

      let collectData path value node = { path, value, children : Array.length $ Tree.children node }

      it "`traverse` on a one-leaf tree" $
        (Path.traverse collectData $ ql "a")
        `compareTrees`
        (ql { path : Path [], value : "a", children : 0 })
      it "`traverse` on a node with children" $
        (Path.traverse collectData
          $ "a" :<~ [ "b", "c", "d", "e" ]
        )
        `compareTrees`
        ({ path : Path [], value : "a", children : 4 } :<~
          [ { path : Path [0], value : "b", children : 0 }
          , { path : Path [1], value : "c", children : 0 }
          , { path : Path [2], value : "d", children : 0 }
          , { path : Path [3], value : "e", children : 0 }
          ]
        )
      it "`traverse` on a node with children 2" $
        (Path.traverse collectData
          $ "a" :< [ ql "b", ql "c", "d" :< [], ql "e" ]
        )
        `compareTrees`
        ({ path : Path [], value : "a", children : 4 } :<
          [ ql { path : Path [0], value : "b", children : 0 }
          , ql { path : Path [1], value : "c", children : 0 }
          ,    { path : Path [2], value : "d", children : 0 } :< []
          , ql { path : Path [3], value : "e", children : 0 }
          ]
        )
      it "`traverse` on a node with deeper children" $
        (Path.traverse collectData
          $ "a" :< [ ql "b", ql "c", "d" :< [ ql "q", ql "r", ql "s" ], ql "e" ]
        )
        `compareTrees`
        ({ path : Path [], value : "a", children : 4 } :<
          [ ql { path : Path [0], value : "b", children : 0 }
          , ql { path : Path [1], value : "c", children : 0 }
          , { path : Path [2], value : "d", children : 3 } :<~
            [ { path : Path [2,0], value : "q", children : 0 }
            , { path : Path [2,1], value : "r", children : 0 }
            , { path : Path [2,2], value : "s", children : 0 }
            ]
          , ql { path : Path [3], value : "e", children : 0 }
          ]
        )

    describe "`find`" $ do

      let

        -- findIn :: forall (m :: Type -> Type) a. MonadThrow Ex.Error m => Show a => Tree a -> Path -> Tree a -> m Unit
        findIn tree path expected =
          case Path.find path tree of
            Just found ->
              found `compareTrees` expected
            Nothing -> fail $ showTree expected <> " wasn't found at " <> show path

        -- failToFind :: forall (m :: Type -> Type) a. MonadThrow Ex.Error m => Show a => Tree a -> Path -> m Unit
        failToFind tree path =
          case Path.find path tree of
            Just what ->
              fail $ "expected to find nothing at " <> show path <> ", but found " <> showTree what
            Nothing -> pure unit

      it "`find` on a one-leaf tree" $
        findIn (ql "a") (Path []) (ql "a")
      it "`find` on a node with children" $ do
        let tree = "a" :<~ [ "b", "c", "d", "e" ]
        findIn tree (Path []) ("a" :<~ [ "b", "c", "d", "e" ])
        findIn tree (Path [1]) (ql "c")
        findIn tree (Path [3]) (ql "e")
      it "`find` on a node with children 2" $ do
        let tree = "a" :< [ ql "b", ql "c", "d" :< [], ql "e" ]
        findIn tree (Path []) ("a" :< [ ql "b", ql "c", "d" :< [], ql "e" ])
        findIn tree (Path [1]) (ql "c")
        findIn tree (Path [2]) ("d" :< [])
        findIn tree (Path [3]) (ql "e")
      it "`find` on a node with deeper children" $ do
        let tree = "a" :< [ ql "b", ql "c", "d" :<~ [ "q", "r", "s" ], ql "e" ]
        findIn tree (Path []) ("a" :< [ ql "b", ql "c", "d" :<~ [ "q", "r", "s" ], ql "e" ])
        findIn tree (Path [1]) (ql "c")
        findIn tree (Path [2]) ("d" :<~ [ "q", "r", "s" ])
        findIn tree (Path [3]) (ql "e")
        findIn tree (Path [2, 1]) (ql "r")
        findIn tree (Path [2, 2]) (ql "s")
        failToFind tree (Path [1, 1])
        failToFind tree (Path [2, 7])

    describe "`edges`" $ do

        it "`edges` of the tree node" $ do
          let tree = "a" :< [ ql "b", ql "c", "d" :<~ [ "q", "r", "s" ], ql "e" ]
          Tree.edges tree `shouldEqual` [ "a" /\ "b", "a" /\ "c", "a" /\ "d", "a" /\ "e", "d" /\ "q", "d" /\ "r", "d" /\ "s" ]


compareTrees ∷ forall (m :: Type -> Type) (a ∷ Type) (b ∷ Type). MonadThrow Ex.Error m => Show a => Show b => Tree a -> Tree b -> m Unit
compareTrees treeA treeB =
  (showTree treeA) `shouldEqual` (showTree treeB)