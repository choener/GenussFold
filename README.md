[![Build Status](https://travis-ci.org/choener/GenussFold.svg?branch=master)](https://travis-ci.org/choener/GenussFold)

# GenussFold: RNA Pseudoknot Folding

[*generalized ADPfusion Homepage*](http://www.bioinf.uni-leipzig.de/Software/gADP/)

The implementation makes use of the *gADP* technique and provides a larger
example on how to implement algorithms that require interleaved, split
syntactic variables.



As an example, consider palindromic brackets `((()))`. Given two types of
brackets, these can be interleaved: `((( [[[ ))) ]]]`. Such interleaved,
long-range dependencies have been observed in human languages and, in
particular, in RNA bioinformatics.

RNA structures may form so-called pseudoknots, where the RNA structure does not
yield a planar structure (the canonical secondary structure) anymore, but
rather forms graphs with crossing edges. Using the idea of interleaved brackets
and given an input sequence `AAA CCC UUU GGG` (with artificial white space to
make this more clear), a pseudoknotted structure may be formed:

``
AAA CCC UUU GGG
[[[ ((( ]]] )))
``

A formal grammar that parses such a structure requires the ability to denote
that a sub-structure has a "hole". We can write such a grammar as follows:
``
S     -> U V U V
<U,U> -> [ε,ε]
<U,U> -> [a,-] <U,U> [-,u]
<V,V> -> [ε,ε]
<V,V> -> [c,-] <V,V> [-,g]
``

The `PKN` grammar in GenussFold (for Genus-1 structures, but much more
pleasurable to write) offers the required features:

1. state that a syntactic variable is split between two regions `<U,U>`
1. state that this split system is linearized and different symbols can be interleaved: `U V U V`
1. in addition, we allow syn



#### Contact

Christian Hoener zu Siederdissen  
Leipzig University, Leipzig, Germany  
choener@bioinf.uni-leipzig.de  
http://www.bioinf.uni-leipzig.de/~choener/  

