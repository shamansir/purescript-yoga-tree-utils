module Yoga.Tree.Extended.Path where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (index, uncons, mapWithIndex, snoc, length, reverse, snoc, dropEnd) as Array
import Data.Tuple.Nested ((/\), type (/\))
import Data.String (joinWith) as String

-- TODO: duplicates `Zipper.Loc` / depth first ?
-- TODO: if not, use `Zipper.Loc` with `Path`


import Yoga.Tree (Tree)
import Yoga.Tree.Extended (break, children, node, value) as YX


newtype Path = Path (Array Int)


root :: Path
root = Path []


advance :: Int -> Path -> Path
advance n (Path path) = Path $ Array.snoc path n


up :: Path -> Path
up (Path path) = Path $ Array.dropEnd 1 path


toArray :: Path -> Array Int
toArray (Path array) = array


fill :: forall a. Tree a -> Tree (Path /\ a)
fill = YX.break \a -> YX.node (Path [] /\ a) <<< Array.mapWithIndex (fill' [])
    where
        fill' path idx =
            let curPath = Array.snoc path idx
            in YX.break \a -> YX.node (Path curPath /\ a) <<< Array.mapWithIndex (fill' curPath)
    -- TODO: same as `traverse \path _ _ -> path`


find :: forall a. Path -> Tree a -> Maybe (Tree a)
find (Path path) =
    if (Array.length path > 0)
    then flip find' path
    else Just
    where
        find' focus rempath =
            Array.uncons rempath >>=
                \{ head, tail } ->
                    Array.index (YX.children focus) head >>=
                        \child ->
                            if (Array.length tail > 0) then
                                find' child tail
                            else
                                Just child


with :: forall a. Path -> (Tree a -> Tree a) -> Tree a -> Tree a
with (Path path) f =
    if (Array.length path > 0)
    then flip with' path
    else f
    where
        with' node rempath =
            case Array.uncons rempath of
                Just { head, tail } ->
                    YX.node (YX.value node)
                        $ Array.mapWithIndex (\idx child ->
                            if idx == head then
                                if (Array.length tail > 0) then
                                    with' child tail
                                else
                                    f child
                            else child
                        )
                        $ YX.children node
                Nothing -> node


traverse :: forall a b. (Path -> a -> Tree a -> b) -> Tree a -> Tree b
traverse f =
    traverse' []
    where
        traverse' path node =
            YX.break
                (\n ->
                    YX.node (f (Path path) n node)
                        <<< Array.mapWithIndex
                            (traverse' <<< Array.snoc path)
                )
                node


instance Show Path where
    show (Path path) =
        case path of
            [] -> "*" -- "<root>"
            indices -> String.joinWith ":" $ show <$> indices