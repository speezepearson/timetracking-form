module Tree.Json exposing (..)

import Json.Decode as JD
import Json.Encode as JE

import Tree exposing (Tree)

encodeTree : (a -> JE.Value) -> Tree a -> JE.Value
encodeTree encodeLabel tree =
  JE.object
    [ ( "value", encodeLabel (Tree.label tree) )
    , ( "children", JE.list (encodeTree encodeLabel) (Tree.children tree) )
    ]

treeDecoder : JD.Decoder a -> JD.Decoder (Tree a)
treeDecoder nodeDecoder =
  JD.map2 Tree.tree
    (JD.field "value" nodeDecoder)
    (JD.field "children" <| JD.list <| JD.lazy (\() -> treeDecoder nodeDecoder))
