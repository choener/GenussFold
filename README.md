![github action: master](https://github.com/choener/GenussFold/actions/workflows/ci.yml/badge.svg)
![github action: master](https://github.com/choener/GenussFold/actions/workflows/hackage.yml/badge.svg)

# GenussFold: RNA Pseudoknot Folding

[*generalized Algebraic Dynamic Programming Homepage*](http://www.bioinf.uni-leipzig.de/Software/gADP/)

The implementation makes use of the *gADP* technique and provides a larger
example on how to implement algorithms that require interleaved, split
syntactic variables.

Formal background can be found in this paper:

Maik Riechert, Christian Höner zu Siederdissen, and Peter F. Stadler  
*Algebraic dynamic programming for multiple context-free languages*  
2015, submitted  
[preprint](http://www.bioinf.uni-leipzig.de/Software/gADP/preprints/rie-hoe-2015.pdf)  



As an example, consider palindromic brackets `((()))`. Given two types of
brackets, these can be interleaved: `((( [[[ ))) ]]]`. Such interleaved,
long-range dependencies have been observed in human languages and, in
particular, in RNA bioinformatics.

RNA structures may form so-called pseudoknots, where the RNA structure does not
yield a planar structure (the canonical secondary structure) anymore, but
rather forms graphs with crossing edges. Using the idea of interleaved brackets
and given an input sequence `AAA CCC UUU GGG` (with artificial white space to
make this more clear), a pseudoknotted structure may be formed:

```
AAA CCC UUU GGG  
[[[ ((( ]]] )))
```

A formal grammar that parses such a structure requires the ability to denote
that a sub-structure has a "hole". We can write such a grammar as follows:

```
S     -> U V U V  
<U,U> -> [ε,ε]  
<U,U> -> [S,-] [a,-] <U,U> [-,S] [-,u]  
<V,V> -> [ε,ε]  
<V,V> -> [S,-] [c,-] <V,V> [-,S] [-,g]
```

The `PKN` grammar in GenussFold (for genus-1 structures, but much more
pleasurable to write) offers the required features:

1. state that a syntactic variable is split between two regions `<U,U>`
1. state that this split system is linearized and different symbols can be
   interleaved: `U V U V`
1. in addition, we allow syntactic variables of lower dimension (like `S`) to
   be used in dimensional stacks of symbols (`[S,-]`).

This system allows writing monotone multiple context-free grammars with good
performance -- we are reasonably close to C in running time performance.
Reasonable means around a factor of 2 slower.



#### Performance comparison

C-code for running time performance comparison is available in the GenussFold
github repository. The direct URL is:
<https://github.com/choener/GenussFold/blob/master/C/genussfold.c>

#### Contact

Christian Hoener zu Siederdissen  
Leipzig University, Leipzig, Germany  
choener@bioinf.uni-leipzig.de  
http://www.bioinf.uni-leipzig.de/~choener/  

