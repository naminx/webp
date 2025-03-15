module WebP.Types ( webPFree
                  ) where

import           Foreign.Ptr (FunPtr, Ptr)

foreign import ccall unsafe "webp/types.h &WebPFree" webPFree :: FunPtr (Ptr a -> IO ())
