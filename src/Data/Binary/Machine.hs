module Data.Binary.Machine where

import Data.ByteString (ByteString)
import Data.Binary.Get (Decoder(..), Get, pushChunk, runGetIncremental)
import Data.Binary.Put (Put, runPut)
import Data.Machine (MachineT(..), ProcessT, Step(Await, Yield), Is(Refl), auto, stopped)

import qualified Data.ByteString.Lazy as Lazy

streamGet :: Monad m => Get a -> ProcessT m ByteString a
streamGet getA = processDecoder' (runGetIncremental getA) (streamGet getA)

processGet :: Monad m => Get a -> ProcessT m ByteString a
processGet getA = processDecoder (runGetIncremental getA)

processDecoder :: Monad m => Decoder a -> ProcessT m ByteString a
processDecoder decA = processDecoder' decA stopped

processDecoder' :: Monad m => Decoder a -> ProcessT m ByteString a -> ProcessT m ByteString a
processDecoder' decA r = MachineT . return $ Await f Refl stopped where
  f xs = case pushChunk decA xs of
    -- TODO Add `Fail` case
    Fail _ _ s    -> error s
    Done _ _ a    -> MachineT . return $ Yield a r
    decA'         -> processDecoder' decA' r
    -- decA'         -> error "foo"

processPut :: Monad m => (a -> Put) -> ProcessT m a ByteString
processPut f = auto $ Lazy.toStrict . runPut . f
