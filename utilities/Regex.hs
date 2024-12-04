-- | [POSIX regular expressions](https://pubs.opengroup.org/onlinepubs/9799919799/basedefs/V1_chap09.html).
module Regex (
    -- * Regular expressions
    Regex,
    basicRegex,
    extendedRegex,
    -- * Matching
    Matches,
    matchOnce,
    matchAll
    ) where

import Data.Maybe
import Foreign
import Foreign.C.String
import Foreign.C.Types
import System.IO.Unsafe

-- Counterparts of POSIX regex types

-- counterpart of regex_t
data CRegex

-- counterpart of regmatch_t
data CRegmatch = CRegmatch {
    regStart :: CRegoff,
    regEnd :: CRegoff
} deriving Show

instance Storable CRegmatch where
    sizeOf _ = sizeOfCRegmatch
    alignment _ = alignOfCRegmatch

    peek ptr = do
        start <- peek (ptr `plusPtr` offsetOfRegStart)
        end <- peek (ptr `plusPtr` offsetOfRegEnd)
        return (CRegmatch start end)

    poke ptr (CRegmatch start end) = do
        poke (ptr `plusPtr` offsetOfRegStart) start
        poke (ptr `plusPtr` offsetOfRegEnd) end

-- counterpart of regoff_t
type CRegoff = CInt

-- Constants from regex.h: may differ between systems

-- REG_EXTENDED: Support extended regular expressions
regExtended :: CInt
regExtended = 1

-- sizeof(regex_t)
sizeOfCRegex :: Int
sizeOfCRegex = 64

-- sizeof(regmatch_t)
sizeOfCRegmatch :: Int
sizeOfCRegmatch = 2 * sizeOf (undefined :: CRegoff)

-- alignof(regmatch_t)
alignOfCRegmatch :: Int
alignOfCRegmatch = alignment (undefined :: CRegoff)

-- offsetof(regmatch_t, rm_so)
offsetOfRegStart :: Int
offsetOfRegStart = 0

-- offsetof(regmatch_t, rm_eo)
offsetOfRegEnd :: Int
offsetOfRegEnd = sizeOf (undefined :: CRegoff)

-- POSIX regex functions

foreign import ccall "regex.h regcomp"
    c_regcomp :: Ptr CRegex -> CString -> CInt -> IO CInt
foreign import ccall "regex.h regexec"
    c_regexec ::
        Ptr CRegex -> CString -> CSize -> Ptr CRegmatch -> CInt -> IO CInt
foreign import ccall "regex.h &regfree"
    c_regfree :: FunPtr (Ptr CRegex -> IO ())


-- | A compiled regular expression
newtype Regex = Regex (ForeignPtr CRegex)

-- | Create a regex object from a string using POSIX Basic Regular Syntax.
basicRegex :: String -> Maybe Regex
basicRegex = makeRegex False

-- | Create a regex object from a string using POSIX Extended Regular Syntax.
extendedRegex :: String -> Maybe Regex
extendedRegex = makeRegex True

-- Create a regex object from a string
makeRegex :: Bool -> String -> Maybe Regex
makeRegex extended pattern = unsafePerformIO $ do
    fp <- mallocForeignPtrBytes sizeOfCRegex
    withForeignPtr fp $ \ regex ->
        withCString pattern $ \ cstr -> do
            result <- c_regcomp regex cstr flag
            if result == 0
                then do
                    addForeignPtrFinalizer c_regfree fp
                    return (Just (Regex fp))
                else do
                    finalizeForeignPtr fp
                    return Nothing
  where
    flag
      | extended = regExtended
      | otherwise = 0

-- Maximum number of captures
maxMatches :: Int
maxMatches = 20

-- | A list containing the whole text matched plus any subexpression matches.
type Matches = [String]

-- | Attempt to match a regex against a string. If successful, return
-- the portion before the match, the match and submatches, and the portion
-- after the match.
matchOnce :: Regex -> String -> Maybe (String, Matches, String)
matchOnce (Regex fp) input = unsafePerformIO $
    withForeignPtr fp $ \ regex ->
    withCString input $ \ cstr ->
    allocaArray maxMatches $ \ regmatches -> do
        result <- c_regexec regex cstr (fromIntegral maxMatches) regmatches 0
        if result == 0
            then do
                matches <- getMatches regmatches cstr maxMatches
                return (Just matches)
            else
                return Nothing

getMatches :: Ptr CRegmatch -> CString -> Int -> IO (String, Matches, String)
getMatches regmatches cstr count = do
    mbss <- flip mapM [0..count-1] $ \ i -> do
        capture <- peekElemOff regmatches i
        let start = fromIntegral (regStart capture)
        let end = fromIntegral (regEnd capture)
        if start == -1
            then
                return Nothing
            else do
                s <- peekCStringLen (cstr `plusPtr` start, end-start)
                return (Just s)
    main <- peekElemOff regmatches 0
    let start = fromIntegral (regStart main)
    let end = fromIntegral (regEnd main)
    front <- peekCStringLen (cstr, start)
    back <- peekCString (cstr `plusPtr` end)
    return (front, catMaybes mbss, back)

-- | Succession of non-overlapping matches of a regex against a string,
-- with the strings before and after each match.
matchAll :: Regex -> String -> ([(String, Matches)], String)
matchAll r = aux []
  where
    aux locs s = case matchOnce r s of
        Nothing -> (reverse locs, s)
        Just (front, ms, rest) -> aux ((front, ms):locs) rest
