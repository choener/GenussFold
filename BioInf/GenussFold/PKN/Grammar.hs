
module BioInf.GenussFold.PKN.Grammar where

import ADPfusion.Subword (makeAlgebraProduct)
import FormalLanguage



-- | Define signature and grammar

[formalLanguage|
Verbose

Grammar: PKN
N: S
{-
 - <U,2> is a split non-terminal. <V,V> shall just access <U,U> which halves memory consumption.
 - However, we can not explicitly separate pseudoknots via @pk1/pk2@ anymore.
 -}
N: <U,2>
N: <V,2>
T: c
S: S
S -> unp <<< S c
S -> jux <<< S c S c
S -> nil <<< e
--S -> pse <<< U V U V

<U,U> -> pkk <<< [S,-] -- [c,-] <U,U> [-,S] [-,c]
-- <U,U> -> nll <<< [e,e]
-- we just need to be able to correctly split the two synvars in S -> UVUV
-- TODO <V,V> should not be a full table, rather be a direct lookup.
<V,V> -> idd <<< <U,U>
//
Emit: PKN

|]

makeAlgebraProduct ''SigPKN

