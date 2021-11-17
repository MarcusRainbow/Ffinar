# A financial maths library in Haskell

_ffin ar_ is the Welsh for "border on".

Ffinar, Functional Financial Analytics and Risk, is an interface onto a pricing library. As a border onto a financial library, it uses the non-strict nature of Haskell to create an interface that is simple and efficient, and only does the calculations that are needed. For example, you can supply a volatility surface, but only the smiles that are actually used in the calculation are evaluated. Moreover, the fact that Haskell is a pure functional language allows beta reduction, which means that expensive calculations are only performed once.
