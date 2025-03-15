{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Codec.Picture.WebP ( decodeRgb8
                          , decodeRgba8
                          , encodeRgb8Lossless
                          , encodeRgba8Lossless
                          , encodeRgb8
                          , encodeRgba8
                          , getWebPDimensions
                          , isAnim
                          ) where

import           Codec.Picture                (Image (Image),
                                               PixelBaseComponent, PixelRGB8,
                                               PixelRGBA8)
import           Codec.Picture.WebP.Container
import           Control.Applicative          (pure, (<*>))
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Internal     as BS
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.ByteString.Unsafe       as BS
import           Data.Functor                 ((<$>))
import           Data.Vector.Storable         (Vector, unsafeFromForeignPtr0,
                                               unsafeWith)
import           Data.Word                    (Word8)
import           Foreign.C.Types              (CChar, CFloat, CInt, CSize)
import           Foreign.ForeignPtr           (newForeignPtr)
import           Foreign.Ptr                  (Ptr, castPtr)
import           System.IO.Unsafe             (unsafePerformIO)
import           WebP.Decode
import           WebP.Encode
import           WebP.Types

errAnim :: BS.ByteString -> BS.ByteString
errAnim inp | isAnim (BSL.fromStrict inp) = error "Animated WebP not supported (nor sensible for JuicyPixels)."
            | otherwise = inp

-- | Get dimensions of a WebP image from a bytestring
getWebPDimensions :: BS.ByteString -> Maybe (Int, Int)
getWebPDimensions webpData = 
    unsafePerformIO $ 
    -- Use unsafeUseAsCStringLen to get a pointer to the data and its length
    BS.unsafeUseAsCStringLen webpData $ \(dataPtr, dataSize) -> do
        -- Call webPGetInfo with the data pointer and size
        (success, width, height) <- webPGetInfo (castPtr dataPtr) (fromIntegral dataSize)

        -- If successful, return the dimensions, otherwise return Nothing
        if success
            then return $ Just (fromIntegral width, fromIntegral height)
            else return Nothing

decodeRgb8 :: BS.ByteString -> Image PixelRGB8
decodeRgb8 = decodeJuicyPixels decodeRgb8BS

decodeRgba8 :: BS.ByteString -> Image PixelRGBA8
decodeRgba8 = decodeJuicyPixels decodeRgba8BS

encodeRgba8 :: CFloat -- ^ Quality, @0.0@ to @100.0@
            -> Image PixelRGBA8 -> BS.ByteString
encodeRgba8 = flip (encodeJuicyPixels webPEncodeRGBA 4)

encodeRgb8 :: CFloat -- ^ Quality, @0.0@ to @100.0@
           -> Image PixelRGB8 -> BS.ByteString
encodeRgb8 = flip (encodeJuicyPixels webPEncodeRGB 3)

encodeRgb8Lossless :: Image PixelRGB8 -> BS.ByteString
encodeRgb8Lossless = encodeJuicyPixelsLossless webPEncodeLosslessRGB 3

encodeRgba8Lossless :: Image PixelRGBA8 -> BS.ByteString
encodeRgba8Lossless = encodeJuicyPixelsLossless webPEncodeLosslessRGBA 4

encodeJuicyPixels :: (PixelBaseComponent p ~ Word8)
                  => (Ptr UInt8 -> CInt -> CInt -> CInt -> CFloat -> IO (CSize, Ptr UInt8))
                  -> Int
                  -> Image p
                  -> CFloat
                  -> BS.ByteString
encodeJuicyPixels encoder pxFactor img = encodeAbsBS encoder w h bytes pxFactor
    where (Image w h bytes) = img

encodeJuicyPixelsLossless :: (PixelBaseComponent p ~ Word8)
                          => (Ptr UInt8 -> CInt -> CInt -> CInt -> IO (CSize, Ptr UInt8))
                          -> Int
                          -> Image p
                          -> BS.ByteString
encodeJuicyPixelsLossless encoder pxFactor img = encodeAbsBSLossless encoder w h bytes pxFactor
    where (Image w h bytes) = img

decodeJuicyPixels :: (PixelBaseComponent p ~ Word8)
                  => (BS.ByteString -> (CInt, CInt, BS.ByteString))
                  -> BS.ByteString
                  -> Image p
decodeJuicyPixels decoder = (\(w, h, img) -> Image (fromIntegral w) (fromIntegral h) (bytesToVec img)) . decoder . errAnim
    where bytesToVec = \(BS.PS fp _ l) -> unsafeFromForeignPtr0 fp l

decodeRgb8BS, decodeRgba8BS :: BS.ByteString -> (CInt, CInt, BS.ByteString)
decodeRgb8BS = decodeAbsBS webPDecodeRGB 3
decodeRgba8BS = decodeAbsBS webPDecodeRGBA 4

{-# NOINLINE decodeAbsBS #-}
decodeAbsBS :: (Ptr CChar -> CSize -> IO (Ptr UInt8, CInt, CInt))
            -> CInt
            -> BS.ByteString
            -> (CInt, CInt, BS.ByteString)
decodeAbsBS decoder pxFactor bs = unsafePerformIO $ BS.unsafeUseAsCStringLen bs $ \(p, l) -> do
    (res, w, h) <- decoder p (fromIntegral l)
    let sz = pxFactor * w * h -- bytes
    img <- BS.PS <$> newForeignPtr webPFree (castPtr res) <*> pure 0 <*> pure (fromIntegral sz)
    pure (w, h, img)

{-# NOINLINE encodeAbsBSLossless #-}
encodeAbsBSLossless :: (Ptr UInt8 -> CInt -> CInt -> CInt -> IO (CSize, Ptr UInt8))
                    -> Int
                    -> Int
                    -> Vector Word8
                    -> Int -- ^ Bytes per pixel
                    -> BS.ByteString
encodeAbsBSLossless encoder w h datums pxFactor =
    unsafePerformIO $ unsafeWith datums $ \p -> do
        (resSz, res) <- encoder (castPtr p) (fromIntegral w) (fromIntegral h) (fromIntegral $ pxFactor * w)
        fP <- newForeignPtr webPFree (castPtr res)
        pure (BS.PS fP 0 (fromIntegral resSz))

{-# NOINLINE encodeAbsBS #-}
encodeAbsBS :: (Ptr UInt8 -> CInt -> CInt -> CInt -> CFloat -> IO (CSize, Ptr UInt8))
            -> Int -- ^ Width
            -> Int -- ^ Height
            -> Vector Word8
            -> Int -- ^ Bytes per pixel
            -> CFloat
            -> BS.ByteString
encodeAbsBS encoder w h datums pxFactor quality =
    unsafePerformIO $ unsafeWith datums $ \p -> do
        (resSz, res) <- encoder (castPtr p) (fromIntegral w) (fromIntegral h) (fromIntegral $ pxFactor * w) quality
        fP <- newForeignPtr webPFree (castPtr res)
        pure (BS.PS fP 0 (fromIntegral resSz))
