module Snap where

import Snap.Core
--import Snap.Http.Server (httpServe)
--import Snap.Http.Server.Config
--import Snap.Util.FileServe (getSafePath, serveDirectoryWith, simpleDirectoryConfig)

import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text as T

--import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)

--------------------

getTextParam :: String -> Snap (Maybe T.Text)
getTextParam = fmap (fmap $ decodeUtf8With lenientDecode) . getParam . fromString

redirectString :: String -> Snap ()
redirectString = redirect . fromString

pathString :: String -> Snap () -> Snap ()
pathString = path . fromString

