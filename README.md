# type-depict.io

A repository for visualizing Haskell type signatures

That is, it can [https://type-depict.io](draw a visualization of a Haskell type)

The core idea is representing any Haskell type level signature as a combination of Connections, Embellishments, Groups and Dots

Connections represents the function arrow `->`, Embellishment any sort of type with kind `(* -> *)` or any tuple or listhigher, and Dots being any type with a kind `*`. Groups
represents precedence of a sub-expression that should be visualized first.

![visualizations](https://github.com/mariatsji/type-depict/blob/main/doc/Visual.png?raw=true)

## Known limitations:
Cant parse PolyKinds or FunDeps signatures
