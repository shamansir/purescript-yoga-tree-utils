module Test.Main where

import Prelude

import Data.String (toUpper) as String

import Effect (Effect)
import Effect.Aff (launchAff_)

import Control.Monad.Error.Class (class MonadThrow)
import Effect.Exception (Error) as Ex


import Test.Spec (describe, it, itOnly)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Yoga.Tree (showTree)
import Yoga.Tree.Extended (Tree(..))
import Yoga.Tree.Extended (node, leaf, set, update) as Tree
import Yoga.Tree.Extended.Path (Path(..))
import Yoga.Tree.Extended.Path as Path
import Yoga.Tree.Extended.Path (with) as Tree


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



compareTrees ∷ forall (m :: Type -> Type) (a ∷ Type) (b ∷ Type). MonadThrow Ex.Error m => Show a => Show b => Tree a -> Tree b -> m Unit
compareTrees treeA treeB =
  (showTree treeA) `shouldEqual` (showTree treeB)