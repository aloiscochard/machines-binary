module Data.Binary.Get.Machine where

import Data.ByteString (ByteString)
import Data.Binary.Get (Decoder(..), Get, pushChunk, runGetIncremental)
import Data.Machine (MachineT(..), ProcessT, Step(Await, Yield), Is(Refl), stopped)

processGet :: Monad m => Get a -> ProcessT m ByteString a
processGet getA = processDecoder (runGetIncremental getA)

processDecoder :: Monad m => Decoder a -> ProcessT m ByteString a
processDecoder decA = MachineT . return $ Await f Refl stopped where
  f xs = case pushChunk decA xs of
    -- TODO Add `Fail` case
    Done _ _ a    -> MachineT . return $ Yield a stopped
    decA'         -> processDecoder decA'
