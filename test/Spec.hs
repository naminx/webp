module Main (main) where

import           Codec.Picture.Png  (encodePng)
import           Codec.Picture.WebP
import qualified Data.ByteString    as BS
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
    testGroup "Converts to PNG without throwing an exception"
        [ convRgbPng "test/data/6b3001b4ebfc341ed0359c00586e6ce1.webp"
        , convRgbPng "test/data/d99cc366dfbbf99493f917e2d43c683f.webp"
        -- , convRgbPng "test/data/tumblr_ddca20a372af5d3c6d38436ffc882f72_99c41c0a_1280.webp"
        , convRgbaPng "test/data/6b3001b4ebfc341ed0359c00586e6ce1.webp"
        , convRgbaPng "test/data/d99cc366dfbbf99493f917e2d43c683f.webp"
        , roundtripLosslessRGB "test/data/6b3001b4ebfc341ed0359c00586e6ce1.webp"
        , roundtripLosslessRGB "test/data/d99cc366dfbbf99493f917e2d43c683f.webp"
        , roundtripLosslessRGBA "test/data/6b3001b4ebfc341ed0359c00586e6ce1.webp"
        , roundtripLosslessRGBA "test/data/d99cc366dfbbf99493f917e2d43c683f.webp"
        , roundtripRGB "test/data/6b3001b4ebfc341ed0359c00586e6ce1.webp"
        , roundtripRGB "test/data/d99cc366dfbbf99493f917e2d43c683f.webp"
        , roundtripRGBA "test/data/6b3001b4ebfc341ed0359c00586e6ce1.webp"
        , roundtripRGBA "test/data/d99cc366dfbbf99493f917e2d43c683f.webp"
        ]

convRgbPng :: FilePath -> TestTree
convRgbPng fp = testCase ("Converts to PNG (" ++ fp ++ ")") $ do
    bs <- BS.readFile fp
    let img = decodeRgb8 bs
        conv = encodePng img
    assertBool "No exception" (conv `seq` True)

convRgbaPng :: FilePath -> TestTree
convRgbaPng fp = testCase ("Converts to PNG (" ++ fp ++ ")") $ do
    bs <- BS.readFile fp
    let img = decodeRgba8 bs
        conv = encodePng img
    assertBool "No exception" (conv `seq` True)

roundtripLosslessRGB :: FilePath -> TestTree
roundtripLosslessRGB fp = testCase ("roundtripLosslesss (" ++ fp ++ ")") $ do
    bs <- BS.readFile fp
    let img = decodeRgb8 bs
        conv = encodeRgb8Lossless img
    assertBool "No exception" (conv `seq` True)

roundtripLosslessRGBA :: FilePath -> TestTree
roundtripLosslessRGBA fp = testCase ("roundtripLosslesss (" ++ fp ++ ")") $ do
    bs <- BS.readFile fp
    let img = decodeRgba8 bs
        conv = encodeRgba8Lossless img
    assertBool "No exception" (conv `seq` True)

roundtripRGB :: FilePath -> TestTree
roundtripRGB fp = testCase ("roundtripLosslesss (" ++ fp ++ ")") $ do
    bs <- BS.readFile fp
    let img = decodeRgb8 bs
        conv = encodeRgb8 0.4 img
    assertBool "No exception" (conv `seq` True)

roundtripRGBA :: FilePath -> TestTree
roundtripRGBA fp = testCase ("roundtripLosslesss (" ++ fp ++ ")") $ do
    bs <- BS.readFile fp
    let img = decodeRgba8 bs
        conv = encodeRgba8 0.4 img
    assertBool "No exception" (conv `seq` True)
