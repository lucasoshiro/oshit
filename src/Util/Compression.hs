module Util.Compression where

import qualified Codec.Compression.Zlib     as Zlib
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L

compress :: B.ByteString -> B.ByteString
compress = L.toStrict . Zlib.compress . L.fromStrict

decompress :: B.ByteString -> B.ByteString
decompress = L.toStrict . Zlib.decompress . L.fromStrict

