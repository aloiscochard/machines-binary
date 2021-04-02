{-# LANGUAGE Rank2Types #-}

module Data.Binary.Machine (
  -- * Get
    processGet
  , processGetL
  , streamGet
  , streamGetL
  -- * Put
  , processPut
  -- * Types
  , DecodingError(..)
  ) where

import Data.ByteString (ByteString)
import Data.Binary.Get (Decoder(..), Get, ByteOffset, pushChunk, runGetIncremental)
import Data.Binary.Put (Put, runPut)
import Data.Machine (Plan, ProcessT, Process, auto, repeatedly, yield, echo)
import Data.Machine.Stack (Stack(..), stack, push, pop)

import qualified Data.ByteString.Lazy as Lazy

-- |
-- Construct a Plan that run a 'Get' until it fails or it return a parsed result.
-- This plan automatically manages the pushback of unused input.
--
-- You can use this function to construct a machine and run a 'Get' on the
-- provided input.
-- With 'stack' you can convert the created machine to a normal machine
--
-- @
-- -- construct the machine
-- myMachine :: 'Machine' ('Stack' ByteString) (Either DecodingError Word8)
-- myMachine = 'construct' $ 'processGet' 'getWord8'
--
-- -- run the machine
-- run $ 'stack' ('source' ["abc", "d", "efgh"]) myMachine
-- @
--
-- You can combine machines created in this way with the facilities provided by
-- the machines package.
--
-- @
-- --run m2 after m1
-- myMachine = m1 <> m2
--   where
--     m1 = construct $ processGet (getByteString 5)
--     m2 = construct $ processGet (getByteString 1)
--
-- run $ stack (source ["abc", "d", "efgh"]) myMachine
-- > [Right "abcde",Right "f"]
-- @
processGet :: Get a -> Plan (Stack ByteString) (Either DecodingError a) ()
processGet getA = _getPlan getA >>= pure . fmap snd >>= yield

-- | Same as 'processGet' with additional information about the number
-- of bytes consumed by the 'Get'
processGetL :: Get a -> Plan (Stack ByteString) (Either DecodingError (ByteOffset, a)) ()
processGetL getA = _getPlan getA >>= yield

-- | Run a 'Get' multiple times and stream its results
--
-- @
-- run $ source ["abc", "d", "efgh"] ~> streamGet (getByteString 2)
-- > [Right "ab",Right "cd",Right "ef",Right "gh"]
-- @
streamGet :: Get a -> Process ByteString (Either DecodingError a)
streamGet getA = stack echo (repeatedly $ processGet getA)

-- | Same as 'streamGet' with additional information about the number
-- of bytes consumed by the 'Get'
streamGetL :: Get a -> Process ByteString (Either DecodingError (ByteOffset, a))
streamGetL getA = stack echo (repeatedly $ processGetL getA)

-- | Encode evrery input object with a 'Put'
processPut :: Monad m => (a -> Put) -> ProcessT m a ByteString
processPut f = auto $ Lazy.toStrict . runPut . f

-- | A 'Get' decoding error.
data DecodingError = DecodingError
  { deConsumed :: {-# UNPACK #-} !ByteOffset
    -- ^ Number of bytes consumed before the error
  , deMessage  :: !String
    -- ^ Error message
  } deriving (Show, Read, Eq)

--------------------------------------------------------------------------
-- Internals
_decoderPlan :: Decoder a -> Plan (Stack ByteString) o (Either DecodingError (ByteOffset, a))
_decoderPlan decA = do
  xs <- pop
  case pushChunk decA xs of
    Fail leftovers consumed e -> push leftovers >> pure (Left (DecodingError consumed e))
    Done leftovers consumed a -> push leftovers >> pure (Right (consumed, a))
    decA'                     -> _decoderPlan decA'

_getPlan :: Get a -> Plan (Stack ByteString) o (Either DecodingError (ByteOffset, a))
_getPlan getA = _decoderPlan $ runGetIncremental getA
--------------------------------------------------------------------------
