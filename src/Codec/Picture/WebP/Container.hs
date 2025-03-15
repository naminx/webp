-- https://stackoverflow.com/a/73170213/11296354
-- https://developers.google.com/speed/webp/docs/riff_container

{-# LANGUAGE OverloadedStrings #-}

module Codec.Picture.WebP.Container (isAnim) where

import           Control.Monad        (unless)
import           Data.Binary.Get      (Get, getByteString, runGetOrFail, skip)
import qualified Data.ByteString.Lazy as BSL

-- | Sniff out animated WebP
--
-- @since 0.1.1.0
isAnim :: BSL.ByteString -> Bool
isAnim inp = case runGetOrFail getAnim inp of Left (_, _, e) -> error e; Right (_, _, x) -> x

getAnim :: Get Bool
getAnim = do
    signature 4 "RIFF" "Expected first four bytes to be RIFF"
    skip 4
    signature 4 "WEBP" "Expected WEBP"
    skip 4 *> skip 14
    anim <- getByteString 4
    pure (anim=="ANIM")
  where
    signature n bytes err = do {sig <- getByteString n; unless (sig==bytes) (fail err)}
