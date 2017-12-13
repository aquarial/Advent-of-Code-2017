{-# LANGUAGE OverloadedStrings #-}
module Sponsers where

import           Data.Bits              (xor)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as C8
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base16 as B16

import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import qualified Data.Text.Encoding        as TE
import qualified Data.Text.Encoding.Error  as TEerror

import Data.Char

fromRight x = case x of
                Left e -> error e
                Right xs -> xs

smartyStreets = fromRight $ B64.decode "U2VuZGluZyBDaHJpc3RtYXMgY2FyZHMgdG8gYmFkIGFkZHJlc3Nlcz8K"

cheppers = error "not yet"

xorB a1 a2 = B.pack $ B.zipWith xor a1 (B.concat [a2,a2,a2,a2,a2])

d = TE.decodeUtf8 $ fromRight $ B64.decode "4oqVKEhnZ0pBZ3RUUkVBZUZCOEtDQjBMQjBNWkNSVkJNd3dSSXg4Y0VBb0hIUXdkQXgwSENnPT0sIOKGkeKGkeKGk+KGk+KGkOKGkuKGkOKGkkJBKQ=="

showd = TIO.putStrLn d
-- ⊕(HggJAgtTREAeFB8KCB0LB0MZCRVBMwwRIx8cEAoHHQwdAx0HCg==, ↑↑↓↓←→←→BA)

hggthing = fromRight $ B64.decode "HggJAgtTREAeFB8KCB0LB0MZCRVBMwwRIx8cEAoHHQwdAx0HCg=="

rot13 x = chr $ (ord x - ord 'a' + ord 'u' - ord 'h') `mod`26 + ord 'a'

workingonit = C8.map (\x -> if isAlpha x then rot13 x else x) $ xorB hggthing "konami"

