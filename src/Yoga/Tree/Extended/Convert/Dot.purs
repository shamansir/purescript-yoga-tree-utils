module Yoga.Tree.Extended.Convert.Dot where

import Prelude
import Data.Newtype (class Newtype, unwrap)

import Data.Tuple (uncurry) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.DotLang (Edge(..), EdgeType(..), Graph, Node(..), graphFromElements) as Dot
import Data.DotLang.Class (toText) as Dot
import Data.DotLang.Attr.Node as DotNode
import Data.DotLang.Attr.Edge as DotEdge

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (edges, flatten) as Tree
import Yoga.Tree.Extended.Path (Path)
import Yoga.Tree.Extended.Path (fill)  as Path


newtype DotId = DotId String

derive instance Newtype DotId _


type DotConvert a =
    { toId :: Path -> a -> DotId
    , nodeAttrs :: Path -> a -> Array DotNode.Attr
    , edgeAttrs :: Path /\ a -> Path /\ a -> Dot.EdgeType /\ Array DotEdge.Attr
    }


toDot :: forall a. (a -> String) -> Tree a -> Dot.Graph
toDot toLabel =
    toDot'
        { toId : const (toLabel >>> DotId)
        , nodeAttrs : const $ const []
        , edgeAttrs : const $ const $ Dot.Forward /\ []
        }


toDot' :: forall a. DotConvert a -> Tree a -> Dot.Graph
toDot' convert tree =
    Dot.graphFromElements nodes edges
    where
        treeWithPaths = Path.fill tree
        nodes = Tuple.uncurry makeDotNode <$> Tree.flatten treeWithPaths
        edges = Tuple.uncurry makeDotEdge <$> Tree.edges treeWithPaths
        makeDotNode path a = Dot.Node (unwrap $ convert.toId path a) $ convert.nodeAttrs path a
        makeDotEdge (startP /\ startV) (endP /\ endV) =
            case convert.edgeAttrs (startP /\ startV) (endP /\ endV) of
                (edgeType /\ edgeAttrs) ->
                    Dot.Edge
                        edgeType
                        (unwrap $ convert.toId startP startV)
                        (unwrap $ convert.toId endP endV)
                        edgeAttrs


toDotText :: forall a. (a -> String) -> Tree a -> String
toDotText f = toDot f >>> Dot.toText


toDotText' :: forall a. DotConvert a -> Tree a -> String
toDotText' conv = toDot' conv >>> Dot.toText


dotConvertDefault :: forall a. (a -> String) -> DotConvert a
dotConvertDefault toId =
    { toId : const (toId >>> DotId)
    , nodeAttrs : const $ const []
    , edgeAttrs : const $ const $ Dot.Forward /\ []
    }


dotConvertWithLabel :: forall a. (Path -> a -> DotId) -> (Path -> a -> String) -> DotConvert a
dotConvertWithLabel toDotId toLabel =
    { toId : toDotId
    , nodeAttrs : \path -> pure <<< DotNode.Label <<< DotNode.TextLabel <<< toLabel path
    , edgeAttrs : const $ const $ Dot.Forward /\ []
    }
