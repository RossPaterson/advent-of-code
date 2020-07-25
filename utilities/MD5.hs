-- | MD5 message digest function
module MD5(md5s) where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Unsafe
import Numeric

-- | hexadecimal coding of the MD5 hash of the string
md5s :: String -> String
md5s = concatMap hexByte . md5

hexByte :: Word8 -> String
hexByte n
  | n < 16 = '0':showHex n ""
  | otherwise = showHex n ""

-- | MD5 hash of the string
md5 :: String -> [Word8]
md5 s = unsafeLocalState $
    allocaBytes contextSize $ \ context ->
    allocaBytes digestLength $ \ digest ->
    withCStringLen s $ \ (sp, len) -> do
    md5Init context
    md5Update context sp (fromIntegral len)
    md5Final digest context
    peekArray digestLength digest

data MD5_CTX

contextSize :: Int
contextSize = 4*4 + 8 + 64

digestLength :: Int
digestLength = 16

foreign import ccall "MD5Init"
    md5Init :: Ptr MD5_CTX -> IO ()
foreign import ccall "MD5Update"
    md5Update :: Ptr MD5_CTX -> Ptr CChar -> CInt -> IO ()
foreign import ccall "MD5Final"
    md5Final :: Ptr Word8 -> Ptr MD5_CTX -> IO ()
