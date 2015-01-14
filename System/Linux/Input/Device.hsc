{-# LANGUAGE ForeignFunctionInterface #-}

module System.Linux.Input.Device (
      getDeviceName
    , getDeviceId
    , InputId (..)
    ) where

import Prelude hiding (product)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Unsafe as BSC
import Foreign
import Foreign.C
import System.IO (Handle)
import System.Posix.Types
import System.Posix.IO (handleToFd)

#include <linux/input.h>

foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> Ptr () -> IO CInt

c_ioctl' :: Fd -> CInt -> Ptr d -> IO ()
c_ioctl' f req p =
    throwErrnoIfMinus1_ "ioctl" $
        c_ioctl (fromIntegral f) req (castPtr p)

getDeviceName :: Handle -> IO BSC.ByteString
getDeviceName h = withFd h $ \f->do
    BSC.takeWhile (/='\0') `fmap` ioctlBS f #{const EVIOCGNAME(0)} (BSC.replicate 255 '\0')

ioctlBS :: Fd -> Int -> BSC.ByteString -> IO BSC.ByteString
ioctlBS f req buf = do
    BSC.unsafeUseAsCString buf $ \p -> do
        print $ (sizedIoctl req $ BSC.length buf, #{const EVIOCGNAME(255)})
        c_ioctl' f (sizedIoctl req $ BSC.length buf) p
    return buf

-- | Sets the size of an ioctl request number
sizedIoctl :: Int -> Int -> CInt
sizedIoctl req size =
    fromIntegral $ (req .&. complement (mask `shiftL` shift))
                   .|. ((mask .&. size) `shiftL` shift)
  where
    mask = #{const _IOC_SIZEMASK}
    shift = #{const _IOC_SIZESHIFT}

data InputId = InputId { busType :: Word16
                       , vendor  :: Word16
                       , product :: Word16
                       , version :: Word16
                       }
             deriving (Ord, Eq, Show)

instance Storable InputId where
    sizeOf _ = 8
    alignment _ = 8
    peek p = do
        busType <- peekElemOff p' 0
        vendor  <- peekElemOff p' 1
        product <- peekElemOff p' 2
        version <- peekElemOff p' 3
        return $ InputId busType vendor product version
      where p' = castPtr p :: Ptr Word16

    poke p i = do
        pokeElemOff p' 0 $ busType i
        pokeElemOff p' 1 $ vendor i
        pokeElemOff p' 2 $ product i
        pokeElemOff p' 3 $ version i
      where p' = castPtr p :: Ptr Word16

-- | Invoke ioctl with a Storable argument
ioctlStorable :: Storable a => Fd -> Int -> a -> IO a
ioctlStorable f req a = alloca $ \p -> do
    poke p a
    c_ioctl' f (fromIntegral req) p
    peek p

-- | Invoke ioctl with an uninitialized Storable argument
ioctlStorable' :: Storable a => Fd -> Int -> IO a
ioctlStorable' f req = alloca $ \p -> do
    c_ioctl' f (fromIntegral req) p
    peek p

withFd :: Handle -> (Fd -> IO a) -> IO a
withFd h action = handleToFd h >>= action

getDeviceId :: Handle -> IO InputId
getDeviceId h =
    withFd h $ \f->ioctlStorable' f #{const EVIOCGID}
