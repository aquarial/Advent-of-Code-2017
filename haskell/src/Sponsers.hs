{-# LANGUAGE OverloadedStrings #-}
module Sponsers where

import           Data.Bits              (xor)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as C8
import qualified Data.ByteString.Base64 as B64

smartyStreets :: Either String ByteString
smartyStreets = B64.decode "U2VuZGluZyBDaHJpc3RtYXMgY2FyZHMgdG8gYmFkIGFkZHJlc3Nlcz8K"


-- ??? I'm stuck here
cheppers = case B64.decode "Pz0pQUI7ChcmER8YDAEYAh4LGwEP" of
             Left e    -> error e
             Right msg -> B.pack $ B.zipWith xor msg "↑↑↓↓←→←→BA"

-- B64.decode "Pz0pQUI7ChcmER8YDAEYAh4LGwEP"
a :: ByteString
a = "?=)AB;\n\ETB&\DC1\US\CAN\f\SOH\CAN\STX\RS\v\ESC\SOH\SI"

c :: ByteString
c = "↑↑↓↓←→←→BA"
