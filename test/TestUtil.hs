{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}

module TestUtil where

import Language.Haskell.TH.Syntax
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import GHC.Prim (Addr#)
import GHC.ForeignPtr (newForeignPtr_)
import GHC.Ptr (Ptr(Ptr))

testBytes :: BS.ByteString
testBytes = "test bytes"

bsToBytes :: BS.ByteString -> Bytes
bsToBytes bs =
  Bytes
    { bytesPtr = fp
    , bytesOffset = fromIntegral offset
    , bytesSize = fromIntegral size
    }
  where
    (fp, offset, size) = BS.toForeignPtr bs

addrToBs :: Addr# -> Int -> IO BS.ByteString
addrToBs addr len = do
  fp <- newForeignPtr_ (Ptr addr)
  return $ BS.fromForeignPtr fp 0 len
