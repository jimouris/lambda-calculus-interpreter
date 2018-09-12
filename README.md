# λ-Calculus Interpreter [![License MIT][badge-license]](LICENSE.txt)
A simple lambda calculus interpreter

## 2 Modes:
#### 1) Give lambda expressions:
1. λ : \\
2. e.g. "\\x.\\y.x"
```
> reduceNF (myparse "(\\x.\\y.zxy)w")
["(\\x.\\y.zxy)w", "\\b.zwb", "zw"]

> reduceNF (myparse "(\\x.\\y.zxy)(wy)")
["(\\x.\\y.zxy)(wy)", "\\b.z(wy)b", "z(wy)"]

> reduceNF (myparse "(\\n.\\f.\\x.nf(fx))(\\f.\\x.fx)")
["(\\n.\\f.\\x.nf(fx))(\\f.\\x.fx)", "\\b.\\c.(\\f.\\x.fx)b(bc)", "\\b.\\c.(\\c.bc)(bc)", "\\b.\\c.b(bc)"]
```
#### 2) Give fixed terms (true, false, chuch 2, etc):
1. true       : chTrue
2. false      : chFalse
3. ifthenelse : chCond
4. church num : church
5. succ       : chSucc
6. plus       : chPlus
7. mult       : chMult
8. exp        : chExp
9. iszero     : chIsZero
10. pair      : chPair
11. fst       : chFst
12. snd       : chSnd
13. and       : chAnd
14. or        : chOr
```
> prettyprint (chSucc (church 2))
"\\f.\\x.f(f(fx))"

> prettyprint (chIsZero (church 0))
"\\x.\\y.x"

> prettyprint (chPlus (chSucc (church 2)) (church 3))
"\\f.\\x.f(f(f(f(f(fx)))))"

> prettyprint (chMult (church 2) (church 3))
"\\f.\\b.f(f(f(f(f(fb)))))"

> prettyprint (chExp (church 2) (church 3))
"\\b.\\c.b(b(b(b(b(b(b(bc)))))))"

> prettyprint (chNot chFalse)
"\\x.\\y.x"

> prettyprint (chSnd (chPair (church 2) (church 3)))
"\\f.\\x.f(f(fx))"

> prettyprint (chOr chFalse  chTrue)
"\\x.\\y.x"
```

2015-2016


[badge-license]: https://img.shields.io/badge/license-MIT-green.svg?style=flat-square
