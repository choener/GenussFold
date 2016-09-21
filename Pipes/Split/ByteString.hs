
-- | Split incombing bytestrings based on bytestrings.

module Pipes.Split.ByteString where

import           Control.Monad (join,unless)
import           Control.Monad.Trans.Class (lift)
import           Data.ByteString (ByteString)
import           Data.ByteString.Search (indices)
import           Data.Monoid ((<>))
import           Debug.Trace
import           Pipes (Producer,next,yield)
import qualified Data.ByteString as BS

type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)

-- | Splits bytestrings after each pattern @pat@. Tries to minimize the
-- number of intermediate bytestring constructors.
--
-- The following function @ske@ expects a string @str@ and a pattern @pat@
-- and then returns a tuple with the splitted bytestrings in @fst@ and the
-- return value in @snd@.
--
-- The inner parser @parse@ uses @zoom@ to draw the full inner producer,
-- which should contain just one bytestring, namely one of the split off
-- ones. @parse@ doesn't do anything with the inner producer, except
-- returning the contained bytestring.
--
-- @parse@ returns @Right $ concat xs@ on a correct parse, and @Left []@
-- once the input has been exhausted.
--
-- @
-- ske :: ByteString -> ByteString -> ([ByteString],[ByteString],[ByteString])
-- ske pat str | BS.null pat || BS.null str = ([],[],[])
-- ske pat str =
--   let parse = do
--         xs <- zoom (splitKeepEnd pat) PP.drawAll
--         case xs of
--           [] -> return $ Left []
--           xs -> return $ Right $ BS.concat xs
--       (a,(b,p)) = runIdentity . P.toListM' $ PP.parsed parse $ PP.yield str
--   in (a,b, fst . runIdentity . P.toListM' $ p)
-- @

splitKeepEnd :: Monad m => ByteString -> Lens' (Producer ByteString m x) (Producer ByteString m (Producer ByteString m x))
splitKeepEnd pat k p0 = fmap join (k (go BS.empty p0)) where
  go pre p = do
    x <- lift (next p)
    case x of
      Left r -> return $ return r
      Right (bs, p') -> do
        case fnd (pre <> bs) of
          -- no hit yet, send the bs down, keep some suffix
          [] -> do
            unless (BS.null bs) (yield bs)
            let pfx = BS.drop (BS.length bs - l + 1) bs
            go pfx p'
          -- at least one hit, split off the correct part, remainder goes
          -- back.
          (k:_) -> do
            let (y,suf) = BS.splitAt (k - BS.length pre + l) bs
            yield y
            return (yield suf >> p')
  l = BS.length pat
  fnd = indices pat



-- manual splitting
referenceByteStringTokenizer pat str | BS.null pat || BS.null str = []
referenceByteStringTokenizer pat str
  = (h `BS.append` BS.take (BS.length pat) t)
  : if BS.null t then [] else referenceByteStringTokenizer pat (BS.drop (BS.length pat) t)
    where (h,t) = BS.breakSubstring pat str

