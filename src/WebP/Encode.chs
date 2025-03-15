module WebP.Encode ( webPEncodeRGB
                   , webPEncodeRGBA
                   , webPEncodeLosslessRGB
                   , webPEncodeLosslessRGBA
                   ) where

import Data.Coerce (coerce)
import Foreign.C.Types (CFloat, CInt, CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)

#include <webp/encode.h>

type UInt8 = {# type uint8_t #}

{# fun WebPEncodeRGB as ^ { id `Ptr UInt8', `CInt', `CInt', `CInt', `CFloat', alloca- `Ptr UInt8' peek* } -> `CSize' coerce #}
{# fun WebPEncodeRGBA as ^ { id `Ptr UInt8', `CInt', `CInt', `CInt', `CFloat', alloca- `Ptr UInt8' peek* } -> `CSize' coerce #}
{# fun WebPEncodeLosslessRGB as ^ { id `Ptr UInt8', `CInt', `CInt', `CInt', alloca- `Ptr UInt8' peek* } -> `CSize' coerce #}
{# fun WebPEncodeLosslessRGBA as ^ { id `Ptr UInt8', `CInt', `CInt', `CInt', alloca- `Ptr UInt8' peek* } -> `CSize' coerce #}
