module Data.Binary.Put.Machine where

import Data.ByteString.Lazy (ByteString)
import Data.Binary.Put (Put, runPut)
import Data.Machine

processPut :: Monad m => (a -> Put) -> ProcessT m a ByteString
processPut f = repeatedly $ await >>= g where g = yield . runPut . f
