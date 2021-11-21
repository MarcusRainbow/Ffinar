# A financial maths library in Haskell

_ffin ar_ is the Welsh for "border on".

Ffinar, Functional Financial Analytics and Risk, is an interface onto a pricing library. As a border onto a financial library, it uses the non-strict nature of Haskell to create an interface that is simple and efficient, and only does the calculations that are needed. For example, you can supply a volatility surface, but only the smiles that are actually used in the calculation are evaluated. Moreover, the fact that Haskell is a pure functional language allows beta reduction, which means that expensive calculations are only performed once.

At present, the whole library is implemented in non-strict Haskell, but I have left open the possibility of implementing performance-critical sections in strict Haskell or even some other language such as Rust or C++.

## Programming principles

Everything in Ffinar is non-strict. We use foldr rather than foldl, lists rather than vectors, and try to code things such that unnecessary calculations are not performed. For the rather trivial examples of models implemented so far, this may lead to runtimes actually being longer. However, more expensive calculations such as Monte-Carlo should really benefit (though possibly only if the calculations themselves are strict).

I have tried to keep things functional as far as possible, rather than falling back on object-oriented design such as classes, though I do allow these when appropriate. For example, the key property of an instrument is a Pricer function, rather than a Priceable class (though the latter exists), and risks and other calculations are defined in terms of Pricer rather than Priceable.

## Design principles

The central design is similar to that pioneered by Goldman Sachs. Every instrument can be broken down into a tree of sub-instruments and each instrument has one key method -- price. Unlike Goldman's library, price has a parameter (market), which means that we do not have to go the rather ugly and very non-functional route of singletons to supply market data.

I have aimed at a real-world pricing library that could be used in a bank, rather than a purely academic library. For example, I use discrete cash dividends rather than a yield, and a vol surface parameterisation that is in common use in the city.

I have kept things simple. I have not implemented settlement or business day volatility, though these would be easy to add in the future.

All static data is passed in from the outside. For example, the currency associated with an equity must be passed in with the equity. The library does not know that IBM is USD-based.

## How to use the library

The test files are the main documentation. If you want to calculate risks, look at TestRisks.hs. As a very brief overview, to calculate the price and risks of an instrument, you need to:

* Create the instrument you want to price (see TestEuropean.hs for an example)
* Create the market data you want to price it with (see TestDiscount.hs, TestForward.hs and TestVolatility.hs)
* Create a Market, passing in spots, dividends, carry and rate functions, and vol surfaces (see TestMarket.hs)
* Creat the risk functions you want, passing the underliers and bump sizes (see TestRisks.hs)
* Create a Pricer function by invoking _price_ on the instrument
* For Present Value, pass the Market to the Pricer function
* For each of the risks, pass the Pricer function and the Market to the risk functions

