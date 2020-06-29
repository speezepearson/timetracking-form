module Endpoints exposing
  ( getLastPing
  , writePing
  )

import Http

import Protobuf.Encode
import Protobuf.Decode

import Proto

type alias Endpoint req resp =
  { encoder : req -> Protobuf.Encode.Encoder
  , decoder : Protobuf.Decode.Decoder resp
  , url : String
  }

hit : Endpoint req resp -> (Result Http.Error resp -> msg) -> req -> Cmd msg
hit endpoint toMsg req =
  Http.post
    { url = endpoint.url
    , body = Http.bytesBody "application/octet-stream"
      <| Protobuf.Encode.encode
      <| endpoint.encoder req
    , expect = Protobuf.Decode.expectBytes toMsg (endpoint.decoder)
    }

getLastPing : (Result Http.Error Proto.GetLastPingResponse -> msg) -> Proto.GetLastPingRequest -> Cmd msg
getLastPing =
  hit
    { encoder = Proto.toGetLastPingRequestEncoder
    , decoder = Proto.getLastPingResponseDecoder
    , url = "/get_last_ping"
    }

writePing : (Result Http.Error Proto.WritePingResponse -> msg) -> Proto.WritePingRequest -> Cmd msg
writePing =
  hit
    { encoder = Proto.toWritePingRequestEncoder
    , decoder = Proto.writePingResponseDecoder
    , url = "/write_ping"
    }
