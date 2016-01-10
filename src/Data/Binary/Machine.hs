module Data.Binary.Machine where

import Data.ByteString (ByteString)
import Data.Binary.Get (Decoder(..), Get, pushChunk, runGetIncremental)
import Data.Binary.Put (Put, runPut)
import Data.Machine (MachineT(..), ProcessT, Step(Await, Yield), Is(Refl), auto, stopped)

import qualified Data.ByteString.Lazy as Lazy

streamGet :: Monad m => Get a -> ProcessT m ByteString (Either String a)
streamGet getA = processDecoder' (runGetIncremental getA) (streamGet getA)

processGet :: Monad m => Get a -> ProcessT m ByteString (Either String a)
processGet getA = processDecoder (runGetIncremental getA)

processDecoder :: Monad m => Decoder a -> ProcessT m ByteString (Either String a)
processDecoder decA = processDecoder' decA stopped

processDecoder' :: Monad m => Decoder a -> ProcessT m ByteString (Either String a) -> ProcessT m ByteString (Either String a)
processDecoder' decA r = MachineT . return $ Await f Refl stopped where
  f xs = case pushChunk decA xs of
    Fail _ _ e    -> yield' $ Left e
    Done _ _ a    -> yield' $ Right a
    decA'         -> processDecoder' decA' r
  yield' ea = MachineT . return $ Yield ea r

processPut :: Monad m => (a -> Put) -> ProcessT m a ByteString
processPut f = auto $ Lazy.toStrict . runPut . f
