module Test.Main where

import Prelude

import Data.String (toUpper) as String
import Data.Array (length) as Array
import Data.Maybe (Maybe(..))

import Effect (Effect)
import Effect.Aff (launchAff_)

import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error) as Ex


import Test.Spec (describe, it, itOnly)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Yoga.Tree (showTree)
import Yoga.Tree.Extended (Tree(..))
import Yoga.Tree.Extended (node, leaf, set, update, children) as Tree
import Yoga.Tree.Extended.Path (Path(..))
import Yoga.Tree.Extended.Path as Path
import Yoga.Tree.Extended.Path (with, traverse, find) as Tree


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "purescript-yoga-tree-utils" $ do

    describe "`with`+`set`" $ do


      it "`with` : `set` on the root path" $
        (Tree.with (Path []) (Tree.set 'b') $ Tree.leaf 'a')
        `compareTrees`
        (Tree.leaf 'b')
      it "`with` : `set` with one of the children" $
        (Tree.with (Path [2]) (Tree.set 'x')
          $ Tree.node 'a' $ [ Tree.leaf 'b', Tree.leaf 'c', Tree.leaf 'd', Tree.leaf 'e' ]
        )
        `compareTrees`
        (Tree.node 'a' $ [ Tree.leaf 'b', Tree.leaf 'c', Tree.leaf 'x', Tree.leaf 'e' ])
      it "`with` : `set` with the children that is an empty node" $
        (Tree.with (Path [2]) (Tree.set 'x')
          $ Tree.node 'a' $ [ Tree.leaf 'b', Tree.leaf 'c', Tree.node 'd' [], Tree.leaf 'e' ]
        )
        `compareTrees`
        (Tree.node 'a' $ [ Tree.leaf 'b', Tree.leaf 'c', Tree.node 'x' [], Tree.leaf 'e' ])
      it "`with` : `set` with the children that is a node with leafs" $
        (Tree.with (Path [2]) (Tree.set 'x')
          $ Tree.node 'a' $ [ Tree.leaf 'b', Tree.leaf 'c', Tree.node 'd' [ Tree.leaf '1', Tree.leaf '2', Tree.leaf '3' ], Tree.leaf 'e' ]
        )
        `compareTrees`
        (Tree.node 'a' $ [ Tree.leaf 'b', Tree.leaf 'c', Tree.node 'x' [ Tree.leaf '1', Tree.leaf '2', Tree.leaf '3' ], Tree.leaf 'e' ])
      it "`with` : `set` with the children on a deeper level" $
        (Tree.with (Path [2, 0]) (Tree.set 'x')
          $ Tree.node 'a' $ [ Tree.leaf 'b', Tree.leaf 'c', Tree.node 'd' [ Tree.leaf '1', Tree.leaf '2', Tree.leaf '3' ], Tree.leaf 'e' ]
        )
        `compareTrees`
        (Tree.node 'a' $ [ Tree.leaf 'b', Tree.leaf 'c', Tree.node 'd' [ Tree.leaf 'x', Tree.leaf '2', Tree.leaf '3' ], Tree.leaf 'e' ])
      it "`with` : `set` when child doesn't exist" $
        (Tree.with (Path [2, 7]) (Tree.set 'x')
          $ Tree.node 'a' $ [ Tree.leaf 'b', Tree.leaf 'c', Tree.node 'd' [ Tree.leaf '1', Tree.leaf '2', Tree.leaf '3' ], Tree.leaf 'e' ]
        )
        `compareTrees`
        (Tree.node 'a' $ [ Tree.leaf 'b', Tree.leaf 'c', Tree.node 'd' [ Tree.leaf '1', Tree.leaf '2', Tree.leaf '3' ], Tree.leaf 'e' ])

    describe "`with`+`update`" $ do

      it "`with` : `update` on the root path" $
        (Tree.with (Path []) (Tree.update String.toUpper) $ Tree.leaf "a")
        `compareTrees`
        (Tree.leaf "A")
      it "`with` : `update` with one of the children" $
        (Tree.with (Path [2]) (Tree.update String.toUpper)
          $ Tree.node "a" $ [ Tree.leaf "b", Tree.leaf "c", Tree.leaf "d", Tree.leaf "e" ]
        )
        `compareTrees`
        (Tree.node "a" $ [ Tree.leaf "b", Tree.leaf "c", Tree.leaf "D", Tree.leaf "e" ])
      it "`with` : `update` with the children that is an empty node" $ do
        (Tree.with (Path [2]) (Tree.update String.toUpper)
          $ Tree.node "a" $ [ Tree.leaf "b", Tree.leaf "c", Tree.node "d" [], Tree.leaf "e" ]
        )
        `compareTrees`
        (Tree.node "a" $ [ Tree.leaf "b", Tree.leaf "c", Tree.node "D" [], Tree.leaf "e" ])
      it "`with` : `update` with the children that is a node with leafs" $
        (Tree.with (Path [2]) (Tree.update String.toUpper)
          $ Tree.node "a" $ [ Tree.leaf "b", Tree.leaf "c", Tree.node "d" [ Tree.leaf "q", Tree.leaf "r", Tree.leaf "s" ], Tree.leaf "e" ]
        )
        `compareTrees`
        (Tree.node "a" $ [ Tree.leaf "b", Tree.leaf "c", Tree.node "D" [ Tree.leaf "q", Tree.leaf "r", Tree.leaf "s" ], Tree.leaf "e" ])
      it "`with` : `update` with the children on a deeper level" $
        (Tree.with (Path [2, 0]) (Tree.update String.toUpper)
          $ Tree.node "a" $ [ Tree.leaf "b", Tree.leaf "c", Tree.node "d" [ Tree.leaf "q", Tree.leaf "r", Tree.leaf "s" ], Tree.leaf "e" ]
        )
        `compareTrees`
        (Tree.node "a" $ [ Tree.leaf "b", Tree.leaf "c", Tree.node "d" [ Tree.leaf "Q", Tree.leaf "r", Tree.leaf "s" ], Tree.leaf "e" ])
      it "`with` : `update` when child doesn't exist" $
        (Tree.with (Path [2, 7]) (Tree.update String.toUpper)
          $ Tree.node "a" $ [ Tree.leaf "b", Tree.leaf "c", Tree.node "d" [ Tree.leaf "q", Tree.leaf "r", Tree.leaf "s" ], Tree.leaf "e" ]
        )
        `compareTrees`
        (Tree.node "a" $ [ Tree.leaf "b", Tree.leaf "c", Tree.node "d" [ Tree.leaf "q", Tree.leaf "r", Tree.leaf "s" ], Tree.leaf "e" ])

    describe "`traverse`" $ do

      let collectData path value node = { path, value, children : Array.length $ Tree.children node }

      it "`traverse` on a one-leaf tree" $
        (Tree.traverse collectData $ Tree.leaf "a")
        `compareTrees`
        (Tree.leaf { path : Path [], value : "a", children : 0 })
      it "`traverse` on a node with children" $
        (Tree.traverse collectData
          $ Tree.node "a" $ [ Tree.leaf "b", Tree.leaf "c", Tree.leaf "d", Tree.leaf "e" ]
        )
        `compareTrees`
        (Tree.node { path : Path [], value : "a", children : 4 } $
          [ Tree.leaf { path : Path [0], value : "b", children : 0 }
          , Tree.leaf { path : Path [1], value : "c", children : 0 }
          , Tree.leaf { path : Path [2], value : "d", children : 0 }
          , Tree.leaf { path : Path [3], value : "e", children : 0 }
          ]
        )
      it "`traverse` on a node with children 2" $
        (Tree.traverse collectData
          $ Tree.node "a" $ [ Tree.leaf "b", Tree.leaf "c", Tree.node "d" [], Tree.leaf "e" ]
        )
        `compareTrees`
        (Tree.node { path : Path [], value : "a", children : 4 } $
          [ Tree.leaf { path : Path [0], value : "b", children : 0 }
          , Tree.leaf { path : Path [1], value : "c", children : 0 }
          , Tree.node { path : Path [2], value : "d", children : 0 } []
          , Tree.leaf { path : Path [3], value : "e", children : 0 }
          ]
        )
      it "`traverse` on a node with deeper children" $
        (Tree.traverse collectData
          $ Tree.node "a" $ [ Tree.leaf "b", Tree.leaf "c", Tree.node "d" [ Tree.leaf "q", Tree.leaf "r", Tree.leaf "s" ], Tree.leaf "e" ]
        )
        `compareTrees`
        (Tree.node { path : Path [], value : "a", children : 4 } $
          [ Tree.leaf { path : Path [0], value : "b", children : 0 }
          , Tree.leaf { path : Path [1], value : "c", children : 0 }
          , Tree.node { path : Path [2], value : "d", children : 3 }
            [ Tree.leaf { path : Path [2,0], value : "q", children : 0 }
            , Tree.leaf { path : Path [2,1], value : "r", children : 0 }
            , Tree.leaf { path : Path [2,2], value : "s", children : 0 }
            ]
          , Tree.leaf { path : Path [3], value : "e", children : 0 }
          ]
        )

    describe "`find`" $ do

      let

        findIn :: forall (m :: Type -> Type) a. MonadThrow Ex.Error m => Show a => Tree a -> Path -> Tree a -> m Unit
        findIn tree path expected =
          case Tree.find path tree of
            Just found ->
              found `compareTrees` expected
            Nothing -> fail $ showTree expected <> " wasn't found at " <> show path

        failToFind :: forall (m :: Type -> Type) a. MonadThrow Ex.Error m => Show a => Tree a -> Path -> m Unit
        failToFind tree path =
          case Tree.find path tree of
            Just _ ->
              fail $ "expected to find nothing at " <> show path
            Nothing -> pure unit

      it "`find` on a one-leaf tree" $
        findIn (Tree.leaf "a") (Path []) (Tree.leaf "a")

      it "`find` on a node with children" $ do
        let tree = Tree.node "a" $ [ Tree.leaf "b", Tree.leaf "c", Tree.leaf "d", Tree.leaf "e" ]
        findIn tree (Path []) (Tree.node "a" $ [ Tree.leaf "b", Tree.leaf "c", Tree.leaf "d", Tree.leaf "e" ])
        findIn tree (Path [1]) (Tree.leaf "c")
        findIn tree (Path [3]) (Tree.leaf "e")
      it "`find` on a node with children 2" $ do
        let tree = Tree.node "a" $ [ Tree.leaf "b", Tree.leaf "c", Tree.node "d" [], Tree.leaf "e" ]
        findIn tree (Path []) (Tree.leaf "a")
        findIn tree (Path [1]) (Tree.leaf "c")
        findIn tree (Path [2]) (Tree.node "d" [])
        findIn tree (Path [3]) (Tree.leaf "e")
      it "`find` on a node with deeper children" $ do
        let tree = Tree.node "a" $ [ Tree.leaf "b", Tree.leaf "c", Tree.node "d" [ Tree.leaf "q", Tree.leaf "r", Tree.leaf "s" ], Tree.leaf "e" ]
        findIn tree (Path []) (Tree.node "a" $ [ Tree.leaf "b", Tree.leaf "c", Tree.node "d" [ Tree.leaf "q", Tree.leaf "r", Tree.leaf "s" ], Tree.leaf "e" ])
        findIn tree (Path [1]) (Tree.leaf "c")
        findIn tree (Path [2]) (Tree.node "d" [ Tree.leaf "q", Tree.leaf "r", Tree.leaf "s" ])
        findIn tree (Path [3]) (Tree.leaf "e")
        findIn tree (Path [2, 1]) (Tree.leaf "r")
        findIn tree (Path [2, 2]) (Tree.leaf "s")
        failToFind tree (Path [2, 1])
        failToFind tree (Path [2, 2])



compareTrees ∷ forall (m :: Type -> Type) (a ∷ Type) (b ∷ Type). MonadThrow Ex.Error m => Show a => Show b => Tree a -> Tree b -> m Unit
compareTrees treeA treeB =
  (showTree treeA) `shouldEqual` (showTree treeB)