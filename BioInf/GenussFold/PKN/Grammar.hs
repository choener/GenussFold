
module BioInf.GenussFold.PKN.Grammar where

import ADPfusion.Subword
import FormalLanguage



-- | Define signature and grammar

[formalLanguage|
Verbose

Grammar: PKN
N: S
{-
 - <U,2> is a split non-terminal.
 -
 - We explicitly introduce <U> and <V> as we want to have @pk1@ and @pk2@
 - in place. In principle, we could make use of an intermediate recursive
 - syntactic variable to ease the memory load, but this is simpler.
 -}
N: <U,2>
N: <V,2>
T: c
S: S
S -> unp <<< S c
S -> jux <<< S c S c
S -> nil <<< e
S -> pse <<< U V U V

<U,U> -> pk1 <<< [S,-] [c,-] <U,U> [-,S] [-,c]
<U,U> -> nll <<< [e,e]

<V,V> -> pk2 <<< [S,-] [c,-] <V,V> [-,S] [-,c]
<V,V> -> nll <<< [e,e]
//
Emit: PKN
|]

makeAlgebraProduct ''SigPKN

