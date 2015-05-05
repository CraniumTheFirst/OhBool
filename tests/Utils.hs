module Utils (unsafeCatchException) where

import qualified Control.Exception as Exc
import System.IO.Unsafe (unsafePerformIO)


{-# NOINLINE unsafeCatchException #-}
unsafeCatchException :: a -> Maybe a
unsafeCatchException x = unsafePerformIO $ Exc.catch (x `seq` return (Just x)) handler
    where
    handler exc = return Nothing `const` (exc :: Exc.ErrorCall)
