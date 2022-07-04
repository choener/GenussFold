
-- |

module ADPfusion.Subword.SynVar.Split where

import Data.Proxy
import Data.Strict.Tuple
import Data.Type.Equality
import Data.Vector.Fusion.Stream.Monadic hiding (flatten)
import Data.Vector.Fusion.Util (delay_inline)
import Debug.Trace
import GHC.Exts
import GHC.TypeLits
import Prelude hiding (map,mapM)

import Data.PrimitiveArray hiding (map)

import ADPfusion.Core
import ADPfusion.Subword.Core



--instance
--  () => MkStream m (ls :!: Split uId Fragment (TwITbl m arr c j x)) (Z:.Subword I:.Subword I) where

