{-# LANGUAGE OverloadedStrings #-}

-- Network imports
import Network.HTTP.Types (status200, status404, methodGet, RequestHeaders)
import Network.Socket (SockAddr)
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Network.Wai.Parse as Parse

-- Data processing imports
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.Aeson.Key as AKey
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T


main :: IO ()
main = do
    putStrLn "Starting server on port 8080"
    run 8080 app


app :: Application
app request respond = do
    case (requestMethod request, pathInfo request) of
        ("GET", ["ip"])        -> handleIp (remoteHost request) respond
        ("GET", ["headers"])   -> handleHeaders (requestHeaders request) respond
        ("GET", ["user-agent"]) -> handleUserAgent (requestHeaders request) respond
        ("GET", ["params"])    -> handleParams (queryString request) respond
        (_, ["data"])          -> handleData request respond
        _                      -> respond $ responseLBS
            status404
            [("Content-Type", "application/json")]
            (Pretty.encodePretty $ A.object
                ["error" .= ("Not Found" :: String)])


handleIp :: SockAddr -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleIp addr respond = respond $ responseLBS
    status200
    [("Content-Type", "application/json")]
    (Pretty.encodePretty $ A.object
        ["origin" .= show addr])


handleHeaders :: RequestHeaders -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleHeaders headers respond = respond $ responseLBS
    status200
    [("Content-Type", "application/json")]
    (Pretty.encodePretty $ A.object
        ["headers" .= headersToJson headers])
  where
    headersToJson = A.object . map (\(name, value) ->
        AKey.fromText (T.pack (B8.unpack $ CI.original name)) .= B8.unpack value)


handleUserAgent :: RequestHeaders -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleUserAgent headers respond =
    case lookup "User-Agent" headers of
        Just ua -> respond $ responseLBS
            status200
            [("Content-Type", "application/json")]
            (Pretty.encodePretty $ A.object
                ["user-agent" .= B8.unpack ua])
        Nothing -> respond $ responseLBS
            status200
            [("Content-Type", "application/json")]
            (Pretty.encodePretty $ A.object
                ["user-agent" .= ("" :: String)])


handleParams :: [(B8.ByteString, Maybe B8.ByteString)] -> (Response -> IO ResponseReceived)
    -> IO ResponseReceived
handleParams params respond = respond $ responseLBS
    status200
    [("Content-Type", "application/json")]
    (Pretty.encodePretty $ A.object
        ["args" .= paramsToJson params])
  where
    paramsToJson = A.object . map (\(name, mvalue) ->
        AKey.fromText (T.pack $ B8.unpack name) .= maybe "" B8.unpack mvalue)


handleData :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
handleData request respond = do
    body <- strictRequestBody request
    let contentType = lookup "Content-Type" (requestHeaders request)
    respond $ responseLBS
        status200
        [("Content-Type", "application/json")]
        (Pretty.encodePretty $ A.object
            [ "data" .= L8.unpack body
            , "content_type" .= maybe "" B8.unpack contentType
            ])