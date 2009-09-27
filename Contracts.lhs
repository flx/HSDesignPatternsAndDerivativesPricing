<!-- This is a literate Haskell document - don't be put off by the HTML.  It can be loaded directly in GHCI. -->
<html>
<head>
<title>Composing Contracts</title>
<style>
body { margin-left: 0.25in; }
pre { background-color:#F0FEFF; border:1px solid #AFEFFF;
      font-size:14px; padding:5px; }
code { background-color:#F0FEFF; }

.varop     { color: grey;    }
.keyglyph  { color: #b8860b; }
.keyword   { color: #a020f0; }
.comment,
.comment a { color: #b22222; }
.str,
.chr       { color: #bc8f8f; }
.conid     { color: #228b22; }
.num       { color: orange; }
</style>
</head>
<body>

<h1>Composing Contracts</h1>

<p>
This document is an unofficial example implementation of the system originally described in the paper <a href="http://research.microsoft.com/~simonpj/Papers/financial-contracts/contracts-icfp.htm">
Composing contracts: an adventure in financial engineering</a>, by Simon Peyton Jones, Jean-Marc Eber, and Julian Seward.  Familiarity with both versions of this paper is assumed.
</p><p>
This example implementation is a literate Haskell program, <a href="Contracts.lhs">Contracts.lhs</a>.  Don't be put off by the embedded HTML &mdash; it is an executable Haskell program.  It can be executed directly and experimented with in an interactive Haskell environment such as <a href="http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html#ghci-introduction">GHCI</a>.
</p><p>
The program is completely self contained, depending only on a few GHC libraries.  Compatibility with other Haskell implementations has not been tested.
</p><p>
A <a href="/contractEx">web interface</a> to some of the examples is also available.
</p><p>
Please note that this program is an example, intended only for educational use in its current form.  The core implementation is only 215 lines of code, excluding examples and user interface.  As such, it has many limitations.  See the <a href="#todo">Future work</a> section for further information.
</p><p>
This document and program was developed by, and is copyright &copy; 2007 by <a href="mailto:anton@appsolutions.com">Anton van Straaten</a>.  It may be freely used and copied for educational purposes.  For other uses, please contact the author (this is mainly because it seems like overkill to release under a more general license at this point).
</p>

<h2>Contents</h2>

<ul>
<li><a href="#papers">A note about the original papers</a></li>
<li><a href="#module">Module header</a></li>
<li><a href="#notation">Notational conventions</a></li>
<li><a href="#basictypes">Basic data types</a></li>
<li><a href="#contract">Contract implementation</a></li>
<li><a href="#obs">Observable data type</a></li>
<li><a href="#contract-prims">Primitives for Defining Contracts</a></li>
<ul>
  <li><a href="#derived">Derived combinators</a></li>
</ul>
<li><a href="#obs-prims">Primitives over observables</a></li>
<ul>
  <li><a href="#options">Option contracts</a></li>
</ul>
<li><a href="#processes">Value processes</a></h2>
<ul>
  <li><a href="#valproc-helpers">Value process helpers</a></li>
</ul>
<li><a href="#model">Model</a></li>
<ul>
  <li><a href="#rates">Interest rate model</a></li>
  <li><a href="#disc">'Disc' primitive</a></li>
  <li><a href="#absorb">'Absorb' primitive</a></li>
  <li><a href="#exch">Exchange rate model</a></li>
  <li><a href="#expected">Expected value</a></li>
  <li><a href="#snell">Snell primitive</a></li>
</ul>
<li><a href="#probs">Probability calculation</a></li>
<li><a href="#contract-eval">Compositional valuation semantics for contracts</a></li>
<li><a href="#obs-eval">Valuation semantics for observables</a></li>
<li><a href="#process-prims">Process primitives</a></li>
<li><a href="#examples">Examples</a></li>
<ul>
  <li><a href="#main">Main function</a></li>
  <li><a href="#pretty">Pretty pictures</a></li>
  <ul>
    <li><a href="#zcb-pic">Process lattice for zcb</a></li>
    <li><a href="#expected-chart">Expected value</a></li>
    <li><a href="#ratechart">Interest rate evolution</h4>
  </ul>
</ul>
<li><a href="#todo">Future work</a></li>
<ul>
  <li><a href="#time-handling">Support actual dates and times</a></li>
  <li><a href="#evalarch">Implement monadic contract evaluator</a></li>
  <li><a href="#obs-rep">Enhance observable representation</a></li>
  <li><a href="#horizon-support">Improve contract horizon handling</a></li>
</ul>
<li><a href="#appendixA">Appendix A - Tests</a></li>
<li><a href="#appendixB">Appendix B - HTML table output for value process</a></li>
<li><a href="#appendixC">Appendix C - Graphviz Output</a></li>
<li><a href="#appendixD">Appendix D - Google chart</a></li>
<li><a href="#appendixE">Appendix E - Web interface</a></li>
<ul>
  <li><a href="#ExContr">Serializable example specification</a></li>
</ul>
<li><a href="#appendixF">Appendix F - HAppS server integration</a></li>

<li><a href="#references">References</a></li>
<li><a href="#credits">Credits</a></li>
</ul>

<h2 id="papers">A note about the original papers</h2>

<p>
There are a few significant differences between the first and second papers.  The change with the most impact on the implementation relates to the representation of contract horizons.
</p><p>
In A:5.3, entitled "Implementation in Haskell", a value process representation is described in which the lattice for a value process is stored as a list of random variables "in reverse time order", i.e. horizon first, along with the horizon's timestep number.  This is possible because the first paper uses a simple, definite approach to representing horizons: they are specified by a single date.
</p><p>
However, this implementation is not compatible with some of the design decisions described in the second paper (B).  Since the horizon of a contract can depend on observables other than the date, the second paper introduces a more sophisticated approach to representing horizons: horizons are specified by boolean value processes (type <code>PR Bool</code>) that define contract acquisition regions.
</p><p>
This allows the horizon of a contract to cross over more than one time step, depending on the value of the observable(s) that define the horizon.  The use of boolean value processes "to describe the 'region' in which one can acquire a contract" is described as a "breakthrough" in section 3.6 of the second paper.
</p><p>
Since contracts can have indefinite horizons, and also because some value processes may have no horizon, it's not possible in general to represent a value process horizon-first, as suggested in the first paper.
</p><p>
Since the second paper does not provide an explicit description of the Haskell representation, our implementation uses a variation on the one described in the first paper, with some changes to support the second paper's design.
</p>

<h2 id="module">Module header</h2>

> module Contracts
> -- (renderEx, renderExDefault, ExContr(ExContr)) -- limit exports for use from HAppS
> where

> import List
> import Numeric
> import Control.Monad
> import System
> import Text.XHtml.Strict
> import Data.Unique

<h2 id="notation">Notational conventions</h2>
<p>
Most references in this document are of the form P:S.s, where e.g. A:3.5 refers to paper A, section 3.5.  The papers are listed in the <a href="#references">References</a> section. Papers A and B are the first and second versions of the Composing Contracts paper, and paper C is a paper about the functional reactive programming system Fran.
<p></p>
Notational conventions from B:Fig.1:
</p>
<pre>
c, d, u : Contract
      o : Observable
   t, s : Date, time
      k : Currency
      x : Dimensionless real value
      p : Value process
      v : Random variable
</pre>


<h2 id="basictypes">Basic data types</h2>

> data Currency = USD | GBP | EUR | ZAR | KYD | CHF  deriving (Eq, Show)

<p>
A Date is represented as a pair consisting of the start date/time of a contract, represented by a value of type CalendarTime, and an integer representing the number of time steps since the start of a contract.  For example purposes, the time steps are of unspecified duration, and the CalendarTime type is stubbed out.
</p><p>
Representing the time step as a separate integer is useful when manipulating trees representing the evolution of a process over time, where the time step corresponds to an index into a list.
</p>

> type Date = (CalendarTime, TimeStep)

> type TimeStep = Int
> type CalendarTime = ()

Since the example doesn't use real dates, mkDate cheats and creates a Date from a TimeStep.

> mkDate :: TimeStep -> Date
> mkDate s = ((),s)

Because real dates aren't used, all value processes are assumed to begin at the same time, the zeroth time step, <code>time0</code>.

> time0 :: Date
> time0 = mkDate 0

This simplifies some aspects of the implementation, discussed further under <a href="#time-handling">Support actual dates and times</a>.

<h2 id="contract">Contract implementation</h2>

The representation of a contract is based on A:5.3.

> data Contract =
>     Zero
>   | One  Currency
>   | Give Contract
>   | And  Contract Contract
>   | Or   Contract Contract
>   | Cond    (Obs Bool)   Contract Contract
>   | Scale   (Obs Double) Contract
>   | When    (Obs Bool)   Contract
>   | Anytime (Obs Bool)   Contract
>   | Until   (Obs Bool)   Contract
>   deriving Show


<h2 id="obs">Observable data type</h2>

<p>
"In general, a value of type <code>Obs <i>d</i></code> represents a time-varying quantity of type <code><i>d</i></code>." (A:3.3)
</p><p>
An obvious implementation might involve a function of type <code>(Date -> a)</code>.  However, a "quantity" in this context is not a single value, but rather a random variable, i.e. a set of possible values.  This suggests a function of type <code>(Date -> RV a)</code>.  This would allow an arbitrary observable to be converted to a value process during evaluation of a contract, by applying successive dates to the observable's function.  This would be a valid implementation, but in order to take maximum advantage of the lazy list representation used for value processes, we will actually use the following type:
</p>

> newtype Obs a = Obs (Date -> PR a)

<p>
An observable is thus represented as a function from a starting date to a value process.  The "time-varying" nature of an observable is captured primarily by the value process itself (<code>PR a</code>); the <code>Date</code> in the function's type is simply used to specify the start date for the resulting value process.
</p><p>
For development and debugging purposes, Obs will be showable.  Since it is a function type, this is achieved by applying it to a dummy date and displaying the first slice of the resulting process.  This is only useful for getting a rough idea of the definition of a contract.  See <a href="#obs-rep">Enhance observable representation</a> for further discussion of this.
</p>

> instance Show a => Show (Obs a) where
>   show (Obs o) = let (PR (rv:_)) = o time0 in "(Obs " ++ show rv ++ ")"

<h2 id="contract-prims">Primitives for Defining Contracts</h2>

Define a combinator interface to the Contract datatype.  From B:Fig.2:

> zero :: Contract
> zero = Zero

> one :: Currency -> Contract
> one = One

> give :: Contract -> Contract
> give = Give

> cAnd :: Contract -> Contract -> Contract
> cAnd = And

> cOr :: Contract -> Contract -> Contract
> cOr = Or

> cond :: Obs Bool -> Contract -> Contract -> Contract
> cond = Cond

> scale :: Obs Double -> Contract -> Contract
> scale = Scale

> cWhen :: Obs Bool -> Contract -> Contract
> cWhen = When

> anytime :: Obs Bool -> Contract -> Contract
> anytime = Anytime

> cUntil :: Obs Bool -> Contract -> Contract
> cUntil = Until


<h3 id="derived">Derived combinators</h3>

Other combinators can now be derived from these primitives, e.g.:

> andGive :: Contract -> Contract -> Contract
> andGive c d = c `cAnd` give d


<h2 id="obs-prims">Primitives over observables</h2>

<code>konst x</code> is an observable that has value x at any time.

> konst :: a -> Obs a
> konst k = Obs (\t -> bigK k)

<code>lift f o</code> is the observable whose value is the result of applying f to the value of the observable o.

> lift :: (a -> b) -> Obs a -> Obs b
> lift f (Obs o) = Obs (\t -> PR $ map (map f) (unPr $ o t))

<code>lift<sub>2</sub> o<sub>1</sub> o<sub>2</sub></code> is the observable whose value is the result of applying 
f to the values of the observables <code>o<sub>1</sub> o<sub>2</sub></code>.

> lift2 :: (a -> b -> c) -> Obs a -> Obs b -> Obs c
> lift2 f (Obs o1) (Obs o2) = Obs (\t -> PR $ zipWith (zipWith f) (unPr $ o1 t) (unPr $ o2 t))

"The value of the observable <code>date</code> at date <code>t</code> is just <code>t</code>."

> date :: Obs Date
> date = Obs (\t -> PR $ timeSlices [t])

"All numeric operations lift to the Obs type. The implementation is simple, using lift and 
lift<sub>2</sub>."

> instance Num a => Num (Obs a) where
>   fromInteger i = konst (fromInteger i)
>   (+) = lift2 (+)
>   (-) = lift2 (-)
>   (*) = lift2 (*)
>   abs = lift abs
>   signum = lift signum

One quirk is that we need to define a stub for Eq to support the Num instance.

> instance Eq a => Eq (Obs a) where
>   (==) = undefined

We can't implement Eq on an Observable's function, but we can provide a lifted version of equality:

> (==*) :: Ord a => Obs a -> Obs a -> Obs Bool
> (==*) = lift2 (==)


<code>at</code> is a boolean observable that becomes <code>True</code> at time <code>t</code> (B:3.2)

> at :: Date -> Obs Bool
> at t = date ==* (konst t)

Typeclasses don't work so well for relational operators, so define a separate family of them (B:3.3)

> (%<), (%<=), (%=), (%>=), (%>) :: Ord a => Obs a -> Obs a -> Obs Bool
> (%<)  = lift2 (<)
> (%<=) = lift2 (<=)
> (%=)  = lift2 (==)
> (%>=) = lift2 (>=)
> (%>)  = lift2 (>)


<h3 id="options">Option contracts</h3>

From B:3.4:

> european :: Date -> Contract -> Contract
> european t u = cWhen (at t) (u `cOr` zero)

> american :: (Date, Date) -> Contract -> Contract
> american (t1, t2) u = anytime (between t1 t2) u

> between :: Date -> Date -> Obs Bool
> between t1 t2 = lift2 (&&) (date %>= (konst t1)) (date %<= (konst t2))


<h2 id="processes">Value processes</h2>

A value process <code>PR a</code> is represented as a list of random variables <code>RV a</code>, with the random variable corresponding to the earliest time step appearing first in the list.

> newtype PR a = PR { unPr :: [RV a] } deriving Show

<p>
Note that the "informal type definition" of a value process is given in B:4.1 as <code>PR a = Date -> RV a</code>.  However, this definition should not be taken literally.  Among other things, it is not amenable to efficient list-based recursive processing of entire value processes, since it requires a lookup for access to each successive date.  (This was discovered the hard way in an earlier implementation of this code &mdash; thanks to Chung-chieh Shan for pointing out the advantages of relying pervasively on a lazy list implementation, during the presentation of the earlier version of this code in NYC.)
</p><p>
A random variable <code>RV a</code> describes the possible values for a value process at a particular time step.  For example, the random variable describing the outcome of a dice throw would be <code>[1,2,3,4,5,6]</code>.  Random variables are therefore implemented as simple lists.
</p>

> type RV a = [a]

<h3 id="valproc-helpers">Value process helpers</h3>

<code>takePr</code> truncates a (possibly infinite) value process.

> takePr :: Int -> PR a -> PR a
> takePr n (PR rvs) = PR $ take n rvs

<code>horizonPr</code> determines the number of time steps in a value process. Only terminates for finite value processes.

> horizonPr :: PR a -> Int
> horizonPr (PR rvs) = length rvs

<code>andPr</code> returns True if every value in a value process is true, false otherwise.  Only terminates for finite value processes.

> andPr :: PR Bool -> Bool
> andPr (PR rvs) = and (map and rvs)

<h2 id="model">Model</h2>

<p>
The model specifies the particular semantics for underlying observables such as the evolution of interest rates, exchange rates, and the types of calculation used.  The contract evaluation function, evalC, is parameterized over a model to allow different models to be easily used.
</p><p>
The model itself is implemented as a record of model-specific data and functions which can easily be instantiated by a function such as <code>exampleModel</code> below.  Essentially, this amounts to a poor man's higher-order module.
</p>

> data Model = Model {
>   modelStart :: Date,
>   disc       :: Currency -> (PR Bool, PR Double) -> PR Double,
>   exch       :: Currency -> Currency -> PR Double,
>   absorb     :: Currency -> (PR Bool, PR Double) -> PR Double,
>   rateModel  :: Currency -> PR Double
>   }

Define a specific model which defines the model functions given in the paper.
This would normally be defined in a separate module.

> exampleModel :: CalendarTime -> Model
> exampleModel modelDate = Model {
>   modelStart = (modelDate,0),
>   disc       = disc,
>   exch       = exch,
>   absorb     = absorb,
>   rateModel  = rateModel
>   }

>   where

The example model's functions are defined in the following sections.  Note that these definitions are local to the <code>exampleModel</code> record definition (due to the <code>where</code> clause above).

<h3 id="rates">Interest rate model</h3>
<p>
See B:5.1.  This constructs a lattice containing possible interest rates given a starting rate and an increment per time step.
This "unrealistically regular" model matches that shown in B:Fig.8.  However, it is so simple that some interest rates go negative after a small number of time steps.  A better model is needed for real applications.  Don't use this to model your retirement fund!
</p><p>

>   rates :: Double -> Double -> PR Double
>   rates rateNow delta = PR $ makeRateSlices rateNow 1
>     where
>       makeRateSlices rateNow n = (rateSlice rateNow n) : (makeRateSlices (rateNow-delta) (n+1))
>       rateSlice minRate n = take n [minRate, minRate+(delta*2) ..]

Each currency has different parameters for the interest rate model.  Since the model is not realistic, these parameters are currently entirely arbitrary.

>   rateModels = [(CHF, rates 7   0.8)
>                ,(EUR, rates 6.5 0.25)
>                ,(GBP, rates 8   0.5)
>                ,(KYD, rates 11  1.2)
>                ,(USD, rates 5   1)
>                ,(ZAR, rates 15  1.5)
>                ]

>   rateModel k =
>     case lookup k rateModels of
>       Just x -> x
>       Nothing -> error $ "rateModel: currency not found " ++ (show k)

<h3 id="disc">'Disc' primitive</h3>
<p>
The primitive (disc t k) maps a real-valued random variable at date T, expressed in currency k,
to its "fair" equivalent stochastic value process in the same currency k.  See B:4.4 and B:Fig.7.
</p><p>
A simplifying assumption is that at some point, the boolean-valued process becomes True for an entire RV.  This provides a simple termination condition for the discounting process.
</p>

>   disc :: Currency -> (PR Bool, PR Double) -> PR Double
>   disc k (PR bs, PR rs) = PR $ discCalc bs rs (unPr $ rateModel k)

>     where

>       discCalc :: [RV Bool] -> [RV Double] -> [RV Double] -> [RV Double]
>       discCalc (bRv:bs) (pRv:ps) (rateRv:rs) =
>         if and bRv -- test for horizon
>           then [pRv]
>           else let rest@(nextSlice:_) = discCalc bs ps rs
>                    discSlice = zipWith (\x r -> x / (1 + r/100)) (prevSlice nextSlice) rateRv
>                    thisSlice = zipWith3 (\b p q -> if b then p else q) -- allow for partially discounted slices
>                                  bRv pRv discSlice
>                in thisSlice : rest

prevSlice calculates a previous slice in a lattice by averaging each adjacent pair of values in the specified slice

>       prevSlice :: RV Double -> RV Double
>       prevSlice [] = []
>       prevSlice (_:[]) = []
>       prevSlice (n1:rest@(n2:_)) = (n1+n2)/2 : prevSlice rest

<h3 id="exch">'Absorb' primitive</h3>

<p>
"Given a boolean-valued process <code>o</code>, the primitive <code>absorb<sub>k</sub>(o,p)</code> 
transforms the real-valued process <code>p</code>, expressed in currency <code>k</code>, into another
real-valued process. For any state, the result is the expected value of receiving <code>p</code>'s 
value if the region <code>o</code> will never be <code>True</code>, and receiving zero in the contrary.
In states where <code>o</code> is <code>True</code>, the result is therefore zero."
</p>

>   absorb :: Currency -> (PR Bool, PR Double) -> PR Double
>   absorb k (PR bSlices, PR rvs) =
>     PR $ zipWith (zipWith $ \o p -> if o then 0 else p)
>                  bSlices rvs

<h3 id="exch">Exchange rate model</h3>

This is a stub which always returns 1.

>   exch :: Currency -> Currency -> PR Double
>   exch k1 k2 = PR (konstSlices 1)

The definition of the <code>exampleModel</code> ends here.

<h3 id="expected">Expected value</h3>

The code for <code>absorb</code> above does not obviously deal with the expected value mentioned in the spec.  This is because the expected value of each random variable is implicit in the value process lattice representation: each node in the lattice is associated with a <a href="#probs">probability</a>, and the expected value at a particular date is simply the sum of the product of the value at each node and its associated probability.  The following functions implement this calculation.

> expectedValue :: RV Double -> RV Double -> Double
> expectedValue outcomes probabilities = sum $ zipWith (*) outcomes probabilities

> expectedValuePr :: PR Double -> [Double]
> expectedValuePr (PR rvs) = zipWith expectedValue rvs probabilityLattice

<h3 id="snell">Snell primitive</h3>

Not currently implemented.  The paper describes the following as a possible algorithm:

<ul>
<li>take the final column of the tree (horizon),
<li>discount it back one time step,
<li>take the maximum of that column with the corresponding column of the original tree,
<li>then repeat that process all the way back to the root.
</ul>

<p>
<code>snell<sub>k</sub>(o,p)</code> is the smallest process q (under an ordering relation mention briefly at the end of B:4.6) such that:
</p>

<pre>forall o' . (o => o') => q >= snell<sub>k</sub>(o',q)</pre>

<p>
That is, an American option is the least upper bound of any of the deterministic acquisition choices specified by o', where o' is a sub-region of o.
</p>

<h3 id="probs">Probability calculation</h3>

<p>
Each node in a value process lattice is associated with a probability.
</p><p>
"...in our very simple setting the number of paths from the root to the node is proportional to the probability 
that the variable will take that value."
</p>

> probabilityLattice :: [RV Double]
> probabilityLattice = probabilities pathCounts
>   where

>     probabilities :: [RV Integer] -> [RV Double]
>     probabilities (sl:sls) = map (\n -> (fromInteger n) / (fromInteger (sum sl))) sl : probabilities sls

To calculate the number of paths to each node in a lattice, simply add the number of paths to the pair of parent nodes.
This needs to work with Integers as opposed to Ints, because:
<code>findIndex (\sl -> maximum sl > (fromIntegral (maxBound::Int))) pathCounts ==> Just 67</code>

>     pathCounts :: [RV Integer]
>     pathCounts = paths [1] where paths sl = sl : (paths (zipWith (+) (sl++[0]) (0:sl)))

<h2 id="contract-eval">Compositional valuation semantics for contracts</h2>

<p>
See B:Fig.4.  A Haskell type signature for eval is specified in A:5.3.  It has been modified here to return a <code>PR Double</code>, as specified in the semantics in Figure 4, instead of a ValProc.  (In this implementation, the <code>PR Double</code> type is essentially equivalent to the first paper's ValProc type.)
</p>

> evalC :: Model -> Currency -> Contract -> PR Double
> evalC (Model modelDate disc exch absorb rateModel) k = eval    -- punning on record fieldnames for conciseness
>   where eval Zero           = bigK 0
>         eval (One k2)       = exch k k2
>         eval (Give c)       = -(eval c)
>         eval (o `Scale` c)  = (evalO o) * (eval c)
>         eval (c1 `And` c2)  = (eval c1) + (eval c2)
>         eval (c1 `Or` c2)   = max (eval c1) (eval c2)
>         eval (Cond o c1 c2) = condPr (evalO o) (eval c1) (eval c2)
>         eval (When o c)     = disc   k (evalO o, eval c)
> --      eval (Anytime o c)  = snell  k (evalO o, eval c)
>         eval (Until o c)    = absorb k (evalO o, eval c)

<h2 id="obs-eval">Valuation semantics for observables</h2>

See B:Fig.5.  The evalation function for observables, <code>evalO</code>, converts an observable's function to a value process by applying the function to a start date.

> evalO :: Obs a -> PR a
> evalO (Obs o) = o time0

<h2 id="process-prims">Process primitives</h2>

B:Fig6

> bigK :: a -> PR a
> bigK x = PR (konstSlices x)

> konstSlices x = nextSlice [x]
>   where nextSlice sl = sl : (nextSlice (x:sl))

> datePr :: PR Date
> datePr = PR $ timeSlices [time0]

> timeSlices sl@((s,t):_) = sl : timeSlices [(s,t+1) | _ <- [0..t+1]]

> condPr :: PR Bool -> PR a -> PR a -> PR a
> condPr = lift3Pr (\b tru fal -> if b then tru else fal)

> liftPr :: (a -> b) -> PR a -> PR b
> liftPr f (PR a) = PR $ map (map f) a

> lift2Pr :: (a -> b -> c) -> PR a -> PR b -> PR c
> lift2Pr f (PR a) (PR b) = PR $ zipWith (zipWith f) a b

> lift2PrAll :: (a -> a -> a) -> PR a -> PR a -> PR a
> lift2PrAll f (PR a) (PR b) = PR $ zipWithAll (zipWith f) a b

> lift3Pr :: (a -> b -> c -> d) -> PR a -> PR b -> PR c -> PR d
> lift3Pr f (PR a) (PR b) (PR c) = PR $ zipWith3 (zipWith3 f) a b c

A version of zipWith that handles input lists of different lengths.  This is used to support lifted binary operations such as (+).

> zipWithAll :: (a -> a -> a) -> [a] -> [a] -> [a]
> zipWithAll f (a:as) (b:bs)     = f a b : zipWithAll f as bs
> zipWithAll f as@(_:_) []       = as
> zipWithAll f []       bs@(_:_) = bs
> zipWithAll _ _        _        = []

> instance Num a => Num (PR a) where
>   fromInteger i = bigK (fromInteger i)
>   (+) = lift2PrAll (+)
>   (-) = lift2PrAll (-)
>   (*) = lift2PrAll (*)
>   abs = liftPr  abs
>   signum = liftPr signum

> instance Ord a => Ord (PR a) where
>   max = lift2Pr max

> instance Eq a => Eq (PR a) where
>   (PR a) == (PR b) = a == b


<h2 id="examples">Examples</h2>

Instantiate the example model with what would be the model's starting date, if real dates were used.

> xm :: Model
> xm = exampleModel ()

Define an evaluator specific to the example model and the USD currency.

> evalX :: Contract -> PR Double
> evalX = evalC xm USD

The by-now-infamous zero-coupon bond:

> zcb :: Date -> Double -> Currency -> Contract
> zcb t x k = cWhen (at t) (scale (konst x) (one k))

A contract using the ZCB:

> c1 :: Contract
> c1 = zcb t1 10 USD

The test date for the bond is <code>horizon</code> timesteps from the model's current date:

> t1 :: Date
> t1 = mkDate t1Horizon

> t1Horizon = 3 :: TimeStep

A stripped-down versions of the European option from B:3.4.  That example uses real dates that range over more than two years.  This is a smaller version.  Its results have not been checked.

> c11 :: Contract
> c11 = european (mkDate 2)
>          (zcb (mkDate 20) 0.4 USD `cAnd`
>           zcb (mkDate 30) 9.3 USD `cAnd`
>           zcb (mkDate 40) 109.3 USD `cAnd`
>           give (zcb (mkDate 12) 100 USD))

Evaluate the contract c1, in dollars, to produce a value process:

> pr1 :: PR Double
> pr1 = evalX c1

Access the underlying lattice (list of slices):

> tr1 = unPr pr1

Test of 'cUntil' - implementation of absorbEx is similar to zcb, but uses cUntil instead of cWhen.

> absorbEx t x k = cUntil (konst t %> date) (scale (konst x) (one k))


<h3 id="main">Main function</h3>
<p>
There is no main function.  This program is intended to be run in an interactive Haskell environment such as
<a href="http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci.html#ghci-introduction">GHCI</a>, where 
the above examples, combinators, and evaluation functions can be examined and experimented with.  
</p><p>
A <a href="http://contracts.scheming.org/contractEx">web interface</a> to some of the examples is also available.
</p>

<h3 id="pretty">Pretty pictures</h3>

<h4 id="zcb-pic">Process lattice for zcb</h4>
<p>
The following diagram shows the value process lattice for the contract <code>(zcb (mkDate 3) 10 USD)</code>.
It matches B:Fig.9, except for minor details such as the fetching shade of pink.  It was generated using 
<a href="#appendixC">GraphViz</a>, by the following code.
</p>

> zcbImage = latticeImage pr1 "fig9" "png"

<img src="fig9.png">
</p><p>

<h4 id="expected-chart">Expected value</h4>

The following is a chart of expected value at each timestep.  The implementation uses the Google Chart API.
</p>

> c1ExpectedValueUrl = chartUrl $ expectedValuePr pr1

<!-- the following URL was generated by the above code and pasted here -->
<img src="http://chart.apis.google.com/chart?chs=300x200&amp;cht=lc&amp;chxt=x,y&amp;chxr=0,0,3|1,8.6,10.0&amp;chd=t:0.0,31.8,65.1,100.0">
</p>

<h4 id="ratechart">Interest rate evolution</h4>

This is the short-term interest rate evolution from B:Fig8.

> rateEvolution = latticeImage (takePr (t1Horizon + 1) $ rateModel xm USD) "fig8" "png"

<img src="fig8.png">


<h2 id="todo">Future work</h2>

In its present state, this implementation is very much a prototype, intended mainly to provide a concrete illustration of concepts described in the papers on which it is based.  The core implementation is less than 200 lines of Haskell code, excluding examples and user interface.  As such, it has many limitations and omissions.  Some of the more obvious enhancements that could be made include:

<ul>
<li>Support actual dates and times.
<li>Implement monadic contract evaluator.
<li>Enhance observable representation.
<li>Improve contract horizon handling.
<li>Implement more realistic interest and exchange rate models.
<li>Implement the snell function.
<li>Implement a model using other kinds of numerical methods, such as Monte Carlo.
<li>Implement models for specific observables.
<li>Factoring into modules.
</ul>

Additional detail for selected items follows.

<h3 id="time-handling">Support actual dates and times</h3>
<p>
As described in <a href="#basictypes">Basic data types</a>, time is currently modeled using abstract integer time steps. Adding basic support for real dates and times should not be difficult.  Since many financial contracts do not need to be concerned with time steps smaller than days, the following description will focus only on date handling, but the same design applies to support for real times.
</p><p>
Adding date support requires changes in the following areas:
</p>
<ul>
<li>Value processes should store their start date in order to correctly handle operations involving processes that start at different dates.

<li>Operations on value processes need to check the start dates of the processes they operate on, and proceed accordingly.  For example, when adding two value processes that begin at different dates, only random variables which represent the same time step should be added together.

<li>Functions that rely on the zeroth time step <code>time0</code>, particularly the observable evalution function <code>evalO</code> and the date process <code>datePr</code>, should instead use the start date of the contract evaluator's <code>Model</code> parameter.  This can be achieved by converting the contract evaluator <code>evalC</code> from direct style to monadic style (see next subsection), which will allow the functions in question access to the model's start date.
</ul>

<h3 id="evalarch">Implement monadic contract evaluator</h3>
<p>
The evaluator for contracts is currently a very simple, direct-style implementation.  This is possible in part because of simplifying choices such as the use of time steps without actual dates, as mentioned above.
</p><p>
Many other kinds of enhancements to the implementation are likely to require a more sophisticated evaluator design.  Converting to a monadic evaluator would support such enhancements.  Aside from providing primitives direct access to the model, it would also allow alternate monads and monad transformers to be used to parameterize the evaluator semantics.  Jeff Polakow pointed out that a probability monad could be useful, for example.
</p>

<h3 id="obs-rep">Enhance observable representation</h3>

Two ways in which the representation of observables might be enhanced are as follows:

<ul>
<li>C:4.1 suggests that the type could be designed to support simplification and other changes to the observable over time, as well as efficient handling of time intervals (C:4.2).  The details here depend on the requirements of the system being developed.

<li>As the definition of the <code>Show</code> instance for the <a href="#obs">Observable data type</a> demonstrates, the representation of observables as functions limits the ability to inspect contract definitions. With an embedded DSL, if metadata is not stored along with the DSL terms, then the host language source code may be the only complete specification of embedded terms that involve functions.  This could be addressed by the use of a tag to identify observables.  This might take the form of a sum type representing primitive observables such as constants and dates, with provision for arbitrary named functions for more complex observables.  Such a representation is hinted at in the description of the valuation semantics for observables in B:Fig.5.
</ul>

<h3 id="horizon-support">Improve contract horizon handling</h3>
<p>
The second paper's use of boolean value processes to represent acquisition regions is very general.  The full generality of this model is not exploited by the current implementation.  For example, the <a href="#disc">disc primitive</a> assumes that the horizon of a contract corresponds to a single random variable.  However, contract horizons may be based on more complex observables than the date, and composed contracts may also result in complex scenarios in which a contract's horizon crosses more than one random variable.
</p><p>
In addition, some value processes are infinite and have no horizons.  The system should track this to allow it to prevent attempts to perform non-terminating operations on such contracts.  This may also help in implementing operations that combine contracts with different horizons.
</p>

<h2 id=appendixA>Appendix A - Tests</h2>

<p>
A few small tests that came up during development.
</p><p>

> tolerance = 0.001

Test of constant process:

> testK = andPr $ liftPr (== 100) $ takePr 10 (bigK 100)

Test that a slice in the probability lattice adds up to probability 1.0:

> testProb = (sum $ probabilityLattice !! 100) - 1 < tolerance

Test the result of evaluating the c1 contract

> testPr1 = andPr $ lift2Pr (\a b -> (abs (a - b)) < tolerance)
>                           pr1
>                           (PR [[8.641], [9.246,8.901], [9.709,9.524,9.346], [10,10,10,10]])

Run all tests (all three of them!)

> tests = and [testK
>             ,testProb
>             ,testPr1]


<h2 id=appendixB>Appendix B - HTML table output for value process</h2>

This renders a value process lattice as a kind of pyramid, using an HTML table.

> prToTable pr@(PR rvs) = table << (snd $ foldl renderSlice (0, noHtml) rvs)
>   where

>     horizon = horizonPr pr
>     renderSlice (n, rows) rv = (n+1, rows +++ (tr $ td << (show n)
>                                                   +++ (spacer $ horizon - n)
>                                                   +++ (concatHtml (map renderCell rv))
>                                                   +++ (spacer $ horizon - n + 1)))

>     renderCell v = td ! [theclass "cell", colspan 2] << (showFFloat (Just 2) v "")

> spacer 0 = noHtml
> spacer n = td ! [theclass "sp", colspan n] << noHtml

<h2 id=appendixC>Appendix C - Graphviz Output</h2>

<p>
This code generates graphs which represent a value process lattice.  Currently assumes Double values, constrained by showNode's formatting of the value.
</p><p>
Write out tree as Dot file and run Dot to generate image:
</p>

> latticeImage :: PR Double -> String -> String -> IO ExitCode
> latticeImage pr baseName imageType =
>   do writeTreeAsDot baseName pr
>      runDot baseName imageType

Supports interactive display of generated Dot code.

> printTree :: PR Double -> IO ()
> printTree pr = mapM_ putStrLn (dotGraph (prToDot pr))

Write a value process out as a Dot file.

> writeTreeAsDot :: String -> PR Double -> IO ()
> writeTreeAsDot baseName pr = writeFile (baseName ++ dotExt) $ unlines (dotGraph (prToDot pr))

Run Dot on a file with the specified base name, and generate a graphic file with the specified type.

> runDot :: String -> String -> IO ExitCode
> runDot baseName fileType =
>   system $ concat ["dot -T", fileType,
>                    " -o ", baseName, ".", fileType, " ",
>                    baseName, dotExt]

Convert a (PR Double) to a list of dot node relationships.

> prToDot :: PR Double -> [String]
> prToDot (PR rvs) = rvsToDot rvs

Convert lattice to list of dot node relationships.

> rvsToDot :: [RV Double] -> [String]
> rvsToDot rvs = let numberedRvs = assignIds rvs 1
>                in showNodes numberedRvs ++ treeToDot numberedRvs

> dotExt = ".dot"

Number each of the nodes in a lattice.

> assignIds :: [RV a] -> Int -> [RV (Int, a)]
> assignIds [] n = []
> assignIds (rv:rvs) n = numberList (reverse rv) n : assignIds rvs (n + length rv)

> numberList :: [a] -> Int -> [(Int, a)]
> numberList l n = zip [n .. n + length l] l

showNodes returns a list of "primary" Dot representations of numbered RV nodes, with
each node's value specified as the node's label.  These nodes can then be referenced
repeatedly in the generated Dot code without specifying a label.

> showNodes :: [RV (Int, Double)] -> [String]
> showNodes numberedRvs = concatMap showSlice (numberList numberedRvs 0)
>   where showSlice (n, sl) = ("subgraph Slice" ++ show n ++ " { rank=same")
>                             : (map (\(n,s) -> show n ++ nodeLabel s) sl)
>                             ++ ["SL" ++ (show n) ++ " [label=\"" ++ show n ++ "\" style=solid peripheries=0] }"]

> nodeLabel :: Double -> String
> nodeLabel s = " [label=\"" ++ (showFFloat (Just 2) s "\"]")

generate Dot code for relationships between numbered RV nodes.

> treeToDot :: [RV (Int, a)] -> [String]
> treeToDot [a] = []
> treeToDot (a:b:rest) = dotJoin a (take (length a) b)
>                     ++ dotJoin a (tail b)
>                     ++ treeToDot (b:rest)

> dotJoin :: RV (Int, a) -> RV (Int, a) -> [String]
> dotJoin a b = zipWith (\(m,a) (n,b) -> (show m) ++ " -- " ++ (show n)) a b



> dotGraph :: [String] -> [String]
> dotGraph body = dotGraphHdr ++ (map formatDotStmt body) ++ ["}"]

> dotGraphHdr :: [String]
> dotGraphHdr = ["graph contract_lattice {"
>                 ,"  rankdir=LR;"
>                 ,"  dir=none;"
>                 ,"  node [style=filled color=pink shape=box fontsize=10 width=0.5 height=0.4];"]

> formatDotStmt :: String -> String
> formatDotStmt s = "  " ++ s ++ ";"


<h2 id=appendixD>Appendix D - Google chart</h2>

This generates a URL for the Google Chart API.  Used for expected value chart.

> chartUrl :: [Double] -> String
> chartUrl vs = "http://chart.apis.google.com/chart?chs=300x200&cht=lc&chxt=x,y&chg=20,25,2,5&chxr=0,0,"
>               ++ (show $ length vs - 1)
>               ++ "|1," ++ (showFFloat (Just 1) ymin ",")
>                        ++ (showFFloat (Just 1) ymax "&chd=t:")
>               ++ (concat $ intersperse "," $ map (\y -> showFFloat (Just 1) y "") ys)
>   where (ymin, ymax, ys) = chartScale vs 100

Scale specified list of values to a range between 0 and <code>upper</code>.

> chartScale ys upper =
>   let ymin = minimum ys
>       ymax = maximum ys
>       yrange = ymax - ymin
>       yscale = upper/yrange
>   in (ymin, ymax, map (\y -> (y - ymin) * yscale ) ys)


<h2 id=appendixE>Appendix E - Web interface</h2>
<p>
The following code implements a very simple web interface, which allows a few canned examples to be run and displays the resulting value process lattice images along with a chart of expected value (where appropriate).
</p><p>
At the time of writing, the web interface is running at <a href="http://contracts.scheming.org/contractEx">http://contracts.scheming.org/contractEx</a>.  This URL may change in future.
</p>

<h3 id="ExContr">Serializable example specification</h2>

ExContr is a type to specify examples to be run, which is serialized in the request URL.

> newtype ExContr = ExContr (String, [Double], Bool) deriving (Read,Show,Eq)

> useLatticeImage (ExContr (_, _, b)) = b
>
> webPath = "/home/anton/happs92/public/"
>
> tmpImgPath = "imgtmp/"
>
> baseDotFilename = "pr-lattice"
>
> pageTitle = "Composing contracts - simple charts"

> mkUniqueName :: String -> IO String
> mkUniqueName baseName =
>   do u <- newUnique
>      return $ baseName ++ (show $ hashUnique u)

> renderEx :: ExContr -> IO Html
> renderEx exSpec@(ExContr (contractId, args, lattice)) =
>   let pr = evalEx exSpec
>       expValChart = if contractId == "probs" then noHtml -- expected value is meaningless for the probabilities it relies on
>                     else h3 << "Expected value" +++ image ! [src (chartUrl $ expectedValuePr pr)]
>       imageType = "png"
>   in if useLatticeImage exSpec
>      then do baseName <- mkUniqueName baseDotFilename
>              exitCode <- latticeImage pr (webPath ++ tmpImgPath ++ baseName) imageType
>              let pageContents =
>                    case exitCode of
>                      ExitSuccess -> renderExampleForm exSpec (image ! [src latticeUrl, border 1]) expValChart
>                                      where latticeUrl = "/" ++ tmpImgPath ++ baseName ++ "." ++ imageType
>                      _ -> p << "renderEx: error generating lattice image"
>              return $ renderExamplePage pageContents
>      else return $ renderExamplePage $ renderExampleForm exSpec (prToTable pr) expValChart

> renderExDefault = renderExamplePage $
>                     renderExampleForm (ExContr ("zcb", [fromIntegral t1Horizon, 10], True))
>                                       noHtml noHtml

> renderExamplePage contents = renderPage pageTitle $
>       p ! [align "right"] << anchor ! [href "/Contracts.html"] << "Source code"
>   +++ contents

> renderPage :: (HTML a, HTML b) => a -> b -> Html
> renderPage hdg contents = (header << (styleSheet +++ thetitle << hdg))
>                           +++ (body << (h1 << hdg +++ contents))

> styleSheet :: Html
> styleSheet = thelink ! [rel "stylesheet", thetype "text/css", href "/contracts.css" ] << noHtml

evalEx evaluates the contract specified by ExContr.  Instead of pattern matching on the ExContr here, 
to avoid duplication it uses the <code>examples</code> list which is used in generating the web form.

> evalEx :: ExContr -> PR Double
> evalEx (ExContr (name, args, f)) =
>   case lookup name examples of
>     Just (desc, defaultArgs, f) -> if length args >= length defaultArgs -- ignore extra args
>                                    then f args          -- TODO: could handle argument defaulting here? See getArg.
>                                    else dummyContract
>     _ -> dummyContract
>   where
>     dummyContract = evalX $ zcb time0 0 USD -- TODO: proper error reporting (to web page if appropriate)

Limit server abuse - disallow large lattices in web interface.  The program can easily handle thousands of time steps, but generating a graphic of the resulting lattice produces large files and consumes CPU resources.  To experiment with larger trees, run Contracts.lhs on your own machine.

> sanitize r = min (truncate r) 20

Map an example id to a description, default arguments, and an evaluation function.

> examples =
>   -- Contracts
>   [("zcb",   ("Zero-coupon bond",    [t1Horizon, 10],
>                                           (\(r:x:_) -> evalX $ zcb (mkDate $ sanitize r) x USD)))
>   ,("c11",   ("European option",     [],  (\_       -> evalX c11)))
>   -- Underlyings
>   ,("probs", ("Probability lattice", [9], (\(r:_)   -> let n = sanitize r + 1 in PR $ take n probabilityLattice)))
>   ,("rates", ("Interest rate model", [9], (\(r:_)   -> let n = sanitize r + 1 in takePr n $ rateModel xm USD)))]


> renderExampleForm (ExContr (contractId, args, showImage)) chart1 chart2 =
>   form ! [method "GET", action "/contractEx"]
>     << table << ((tr << (td << "Contract" +++ td << "Horizon" +++ td << "Value" +++ td << "Output"))
>              +++ (tr << ((td $ select ! [name "contract"]
>                                      << (map (\(id, (desc, defaultArgs, _)) ->
>                                                  attrIf (id == contractId) selected (option ! [value id]) << desc)
>                                              examples))
>                      +++ (td << textfield "arg1" ! [value $ getArg contractId args 0, size "10"])
>                      +++ (td << textfield "arg2" ! [value $ getArg contractId args 1, size "10"])
>                      +++ (td << (attrIf      showImage  checked (radio "image" "True")  +++ "Image"))))
>              +++ (tr  << (td << submit "submit" "Draw" +++ spacer 2
>                       +++ td << (attrIf (not showImage) checked (radio "image" "False") +++ "Table"))))
>     +++ chart1 +++ hr +++ chart2

Retrieve the nth argument from the argument array; if not present, retrieve from default args for specified example.

> getArg id l n = if n < length l then show $ l !! n
>                 else case lookup id examples of
>                        Just (_, args, _) -> if n < length args
>                                             then show $ args !! n else ""

attrIf adds the specified attribute to the Html element if the condition is true.
Useful for <code>checked</code> and <code>selected</code> attributes.

> attrIf False attr el = el
> attrIf True  attr el = el ! [attr]


<h2 id=appendixF>Appendix F - HAppS server integration</h2>

<p>
The following module can be used to integrate with the <a href="http://happs.org/">HAppS</a> application server.
This provides a <a href="http://contracts.scheming.org/contractEx">web interface</a> to the system.  
</p><p>
This code is not an executable part of Contracts.lhs.  To use it, it should be extracted to its own file and built with HAppS.  The <code>import Contracts</code> line imports Contracts.lhs (this file).
</p>

<pre>
module Main where

import HAppS.Server.AlternativeHTTP
import HAppS.Server.HTTP.AltFileServe
import Control.Monad.State
import Numeric

import Contracts

instance FromData ExContr where
  fromData = do c    <- look "contract"
                arg1 <- look "arg1"
                arg2 <- look "arg2"
                img  <- look "image"
                return $ ExContr (c, map fst $ readFloat arg1
                                            ++ readFloat arg2, read img)

main :: IO ()
main = do simpleHTTP [dir "contractEx"
                        [withData $ \(ExContr t) ->
                           [anyRequest $ liftIO $ liftM toResponse =<< renderEx (ExContr t)]
                        ,anyRequest $ ok $ toResponse renderExDefault]
                     ,fileServe ["Contracts.html"] "public" -- fileserving
                     ]
</pre>


<h2 id="references">References</h2>
<ol type="A">
<li id="paperA"><a href="http://research.microsoft.com/Users/simonpj/Papers/financial-contracts/contracts-icfp.ps.gz">
Composing contracts: an adventure in financial engineering</a></li>
<li id="paperB"><a href="http://research.microsoft.com/Users/simonpj/Papers/financial-contracts/pj-eber.ps">
How to write a financial contract</a></li>
<li id="paperC"><a href="http://citeseer.ist.psu.edu/elliott97functional.html">
Functional Reactive Animation</a></li>
</ol>

<h2 id="credits">Credits</h2>
<p>
First, thanks to the authors of the original papers - Simon Peyton Jones, Jean-Marc Eber, and Julian Seward - for some fascinating and useful papers.
<p>
Thanks also to Thomas Hartman, Jeff Polakow, Adam Peacock, Chung-Chieh Shan and the organizers and members of the <a href="http://lisp.meetup.com/59/">New York Functional Programmers Meetup Group</a> for their encouragement and support.
</p><p>
The HTML version of this document was generated from <a href="Contracts.lhs">Contracts.lhs</a> using <a href="http://www.cs.york.ac.uk/fp/darcs/hscolour/">hscolour</a> with the -lit and -css options to color only the code fragments.  Worked like a charm.  (The rest of the HTML was written by hand.)
</p>

</body>
</html>
