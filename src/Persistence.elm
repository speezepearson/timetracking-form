port module Persistence exposing
  ( listIntToBytes
  , bytesToListInt
  , saveBytes
  )

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode exposing (Decoder, Step(..), succeed)
import Bytes.Encode

port save : List Int -> Cmd msg

listIntToBytes : List Int -> Bytes
listIntToBytes xs =
  Bytes.Encode.sequence (List.map Bytes.Encode.unsignedInt8 xs)
  |> Bytes.Encode.encode
bytesToListInt : Bytes -> List Int
bytesToListInt bs =
  case Bytes.Decode.decode (list Bytes.Decode.unsignedInt8 (Bytes.width bs)) bs of
    Just ints -> ints
    _ -> Debug.todo "impossible"

saveBytes : Bytes -> Cmd msg
saveBytes bs =
  bytesToListInt bs
  |> Debug.log "saving"
  |> save

list : Decoder a -> Int -> Decoder (List a)
list decoder len =
  Bytes.Decode.loop (len, []) (listStep decoder) |> Bytes.Decode.map List.reverse

listStep : Decoder a -> (Int, List a) -> Decoder (Step (Int, List a) (List a))
listStep decoder (n, xs) =
  if n <= 0 then
    succeed (Done xs)
  else
    Bytes.Decode.map (\x -> Loop (n - 1, x :: xs)) decoder
