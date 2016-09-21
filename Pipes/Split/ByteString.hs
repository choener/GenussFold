
-- | Split incombing bytestrings based on bytestrings.
--
-- TODO put reference tokenizer in here

module Pipes.Split.ByteString where

import           Control.Monad (join)
import           Control.Monad.Trans.Class (lift)
import           Data.ByteString (ByteString)
import           Data.ByteString.Search (indices)
import           Data.Monoid ((<>))
import           Pipes (Producer,next,yield)
import qualified Data.ByteString as BS

type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)

-- | Splits bytestrings after each pattern @pat@. Tries to minimize the
-- number of intermediate bytestring constructors.
--
-- TODO insert sample code as comment

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
            yield bs
            go (BS.drop (BS.length bs - l + 1) bs) p'
          -- at least one hit, split off the correct part, remainder goes
          -- back.
          (k:_) -> do
            let (y,suf) = BS.splitAt (k - BS.length pre + l) bs
            yield y
            return (yield suf >> p')
  l = BS.length pat
  fnd = indices pat

