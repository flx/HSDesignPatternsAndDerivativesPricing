\documentclass{scrartcl}

%include preamble.tex
% ***********************************************************************************************************

\parskip 2ex
\parindent 0pt	
 
\title{Design Patterns in Haskell \\ and Derivatives Pricing}
\author{Felix Matschke}

\begin{document}
\maketitle
\tableofcontents
\clearpage

\section{Preamble}

Derivatives pricing code should be correct and execute fast. Mainly because of the latter it is often connected to writing it in C++. This choice can be criticised: C++ is hard to write and difficult to debug (compared to other languages), which requires a lot of effort to ensure that the first goal -- correctness -- is reached. This energy can be better spent.

In the following text which traces a popular book for derivatives pricing in C++, I am trying to show that by choosing Haskell as the programming language the complexity of derivatives pricing code can be vastly decreased - which is certainly a big factor in the production price of the libraries. Haskell has the reputation of being not a big factor off in terms of execution speed. This will not be benchmarked here.

\section{Haskell}

Haskell is great. Everyone should learn it.

This text will not try to teach Haskell. Haskell looks different on the first look and on the second look. It has a number of concepts and operators that are unusual. The book ``Real World Haskell'' is a good introduction. Concepts that seem relevant will be briefly introduced, but with that brevity these introductions are not destined to stand alone as any kind of reference.

\section{A Simple Monte Carlo Model}
\subsection{The Theory}
Below the commonly known formulas describing stock price evolution and Black-Scholes pricing theory. Given the stock price evolution described by:
\begin{equation}
  dS_t = \mu S_t dt + \sigma S_t dW_t
\end{equation}
and the final payoff function $f$, the present value of a derivative is
\begin{equation}
  e^{-rT}\mathbb{E}(f(S_t))
\end{equation}
if the expectation is calculated under the risk free process
\begin{equation}
  dS_t = r S_t dt + \sigma S_t dW_t
\end{equation}

Following some derivations the price of a vanilla option with terminal payoff $f$ can be written as:

\begin{equation}
  e^{-rT}\mathbb{E}(f(S_0 e^{(r - \frac{1}{2}\sigma^2)T + \sigma \sqrt{T} N(0,1)}))
\end{equation}

So a simple algorithm for a Monte Carlo pricer is to draw $n$ gaussian variables $x$ with a distribution $N(0,1)$ and compute the average of

\[f(S_0 e^{(r - \frac{1}{2}\sigma^2)T + \sigma \sqrt{T} x})\]

\subsection{A simple Implementation of a Monte Carlo call option pricer}

%include SimpleMC1.lhs

\subsection{Concepts introduced}

\begin{description}
\item[Pure Functions] Our pricer function \verb+simpleMC1+ is a pure function -- no state is generated or read outside the parameters passed on, including the random draws passed over in the parameter \verb+sample+. This has the advantage that the function will never return a different value if given the same parameters - something valuable to know, especially if correctness is a concern.
\item[Left Fold] General for and while loops are not commonly used (even if they can be replicated to some extent) - but are mostly replaced by recursion or iterations over lists. The left fold used here (\verb+foldl'+) is the strict version of a left fold of the type
\begin{code}
foldl :: (a -> b -> a) -> a -> [b] -> a
\end{code}
As parameters it takes a function with two parameters (one of type \verb+a+, the second of type \verb+b+, returning a value of type \verb+a+)
\begin{code}
f:: a -> b -> a
\end{code}
and two more values $x$ of type \verb+a+ and a list of values $y_i$ -- all of type \verb+b+ denoted as \verb+[b]+ -- and computes the value of $f(f(f(x, y_0), y_1), y_2) \dots$
The strictness of \verb+foldl'+ avoids that the lazy evaluation of Haskell (which is often advantageous) makes our function stack up unevaluated nested functions calls until -- right at the end of the program -- we want to print the value on screen and all calls are evaluated. Haskell uses lazy evaluation where ever it can, except if told otherwise -- like here.
\item[map] The function \verb+map+ is central to the use of the ubiquitous lists in functional languages. It is a function of type 
\begin{code}
map :: (a -> b) -> [a] -> [b]
\end{code}
that - given a function $f$ with one parameter (of type \verb+a+ and returning a value of type \verb+b+) and a list of values $x_i$ of type \verb+a+ returns the new list \[ [f(x_0), f(x_1), \dots] \]
\item[The main function and the IO monad] Without going into details about monads (a concept to be introduced later) - the main function needs to read and write state, otherwise nothing useful can be achieved. The following points are useful to understand the above code:
\begin{itemize}
\item Everything in the IO Monad is achieved by chaining ``actions'' together in the \verb+do+ construct. 
\item In a \verb+do+ construct, the return values of pure functions are retrieved with \verb+let+: \begin{code}let result = function a b c\end{code}
\item In a \verb+do+ construct, values retrieved from actions that are to be used in pure functions are extracted from their monad with \verb+<-+: \begin{code}result <- action\end{code} The following code:
\begin{code}
b <- liftM read $ getLine
\end{code}
is equivalent to:
\begin{code}
a <- getLine
let b = read a
\end{code}
\item Pure functions like \verb+read+ (transforming a string it into values of - say - type \verb+Int+ or \verb+Double+) need to be ``lifted'' into the monad with \verb+liftM+ if used directly on the output of actions. 
\item If not using \verb+do+, actions (like \verb+putStrLn+, printing a line on the screen) can be chained to the next action with \verb+>>+. 
\begin{code}
action1 >> action2
\end{code}
corresponds to 
\begin{code}
do
  action1
  action2
\end{code}
\end{itemize}
\item[The operator \verb+$+] To avoid myriads of parantheses (like (in (lisp ()))) one can use the operator \verb+$+. It makes it possible to write:
\begin{code}
function a1 $ function2 b $ function3 c
\end{code}
instead of
\begin{code}
function a1 (function2 b (function3 c))
\end{code}
Other than making the code (arguably) more readable, this is equivalent.
\end{description}

\subsection{Critiquing the approach}

Several points of critique come to mind:

\begin{itemize}
\item The call payoff is hard coded - a put would need a new function, as would other payoffs like digitals
\item Stats like standard error or a convergence tables would need major changes
\item Sampling is hard coded - this makes it difficult to integrate other types like antithetic sampling
\item The approach does not allow for an efficient termination condition (iterating until a specific standard error or other arises)
\item Large samples are not possible and will make the stack overflow (the sample is before being passed on and evaluated in the Monte Carlo function)
\end{itemize}

Some of these points don't seem too serious: The overall amount of code is small and easy to oversee - one of the major advantages of Haskell. Implementing a different terminal payoff (e.g. for a put) would not need a tremendous amount of code repetition. But it should nevertheless be avoided.

\section{Generalising Payoff and Option Data}

It would be nice to integrate any types of final payoffs - which should not modify our Monte Carlo routine. A data type that contains the information of a specific payoff and for which this payoff can be evaluated without modifying anything else is the solution to the problem.

\subsection{Implementing a Payoff Class}

Haskell is not object oriented but still has classes -- type classes. If a type is an instance of a type class, a specific set of functions are defined for the type (which makes it actually quite similar to object oriented classes where specific methods are defined for an instance of a specific class.) These will be helpful for defining payoffs. When dealing with different payoffs we want to have a function that tells us, based on the given payoff type and associated data like strike, what the payoff in Dollars would be.

\subsection{The Payoff type class}

%include Payoff.lhs

\subsection{Using the Payoff Class}

%include SimpleMC2.lhs

\subsection{Implementing an Option Class}

It would be nice to bundle all the information that we have about the option to be priced in one data container - the expiry of the option is still sitting outside. We can easily define a record type data type that will help us over this:

\subsection{The Option type class}

%include Option.lhs

\subsection{Using the Option Class}

%include SimpleMC3.lhs

\subsection{Concepts introduced}

\begin{description}
\item[Defining new data types] There are 2 different ways to define a new data type: \verb+type+ and \verb+data+. 
\begin{code}
type Foo1 = (Int, Int)
\end{code}
\verb+type+ declares a type synonym. This makes it easier to define the type signatures or constraints.
\begin{code}
data Foo2 = Foo2 Int Int
\end{code}
\verb+data+ declares a new type via a type constructor - in this case a type that contains two integers. There is a record style syntax for \verb+data+:
\begin{code}
data Foo3 = Foo3 {one:: Int, two :: Int}
\end{code}
which creates the same data constructor function Foo3 of the type
\begin{code}
Foo3 :: Int -> Int -> Foo3
\end{code}
but also the two functions
\begin{code}
one :: Foo3 -> Int
tow :: Foo3 -> Int
\end{code}
which do what you would expect them two: extract the two different integers from Foo3. We could define these ourselves for \verb+Foo2+:
\begin{code}
one (Foo2 a _) = a
two (Foo2 _ a) = a
\end{code}
This is eventually what the record syntax does for us.
\item[Type Classes] Haskell is a strongly typed language - which helps in the way that errors can often be spotted at compile time vs. runtime. It also makes type inference possible: As seen in the code so far, we don't have to specify types very often but only in ambiguous situations. Sometimes we might choose to write the type signature to help the readability of the code -- this is done here for all \verb+SimpleMC+ functions.
\end{description}

\section{Monadic Random Number Generation}

Pure functions in Haskell can only receive data through the parameters and give back results as the result of the function -- they are purposefully not made to read or write any other parts of the memory and cannot call functions that do so.

If we would for example want to implement a simple counter function to which we want to pass an increment and get back the new global count we have no choice other than storing the state outside the function and passing it on -- and to receive it back. It cannot be done via a global variable -- the global variable would be state outside the function that would be accessed.

\begin{code}
increment (dx, state) = (state + dx, state + dx)
\end{code}

If we want to do this a number of times this looks like the following:

\begin{code}
main = do
  let state0 = 0
  let (c1, state1) = increment (10, state0)
  putStrLn $ show c1
  let (c2, state2) = increment (11, state1)
  putStrLn $ show c2
\end{code}

This passing on of the state from one ``action'' to another is implemented in the Monad typeclass: A monad of type ``M a'' stands for a chain of actions that results in the type \verb+a+ if the monad gets evaluated. Now we need the possibilities to ``lift'' values into the monad, and to chain actions together:

\begin{code}
return :: (Monad m) => a -> m a
(>>) :: (Monad m) => m a -> m b -> m b
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
\end{code}

\begin{description}
\item[\verb+return+] is an action that takes a pure functional value and returns the monad that (if evaluated) gives this value back.
\item[The operator \verb+>>+] chains two actions together, dismissing the result of the first. When is this useful? In a parser, the first action might be consuming a required chain of characters in the state (the string of unconsumed characters) before the next actual value to be read.
\item[The operator \verb+>>=+] chains two actions together whereby the second action takes a parameter of the same type as the return type of the first action (hidden in the monad).
\end{description}

Pseudo random number generation is a very good example of state that needs to be carried on. One way of passing on state is the following:

It is possible to elegantly simulate state by creating functions that pass the state on to the next - in a chain of ``actions''. This is implemented in Monads in Haskell - and for random number generation there conveniently is a Monad implementation that wraps a the random number generator from the Gnu Scientific Library.

The following modification of our original code will run with arbitrarily large sample sizes in constant space. Another trick has to be applied to achieve the second goal: the function {\tt sum} is replaced by {\tt foldl' (+) 0} which is the strict version of {\tt foldl}. This ensures that Haskell uses (and garbage collects) the samples that have already been used. We will find out about forcing strictness later.

%include SimpleMC1b.lhs

\subsection{Parameters}

Parameters could be nicely wrapped into a parameters class - implementing it initially for a constant Double:

%include Parameter.lhs

\subsection{Using the Parameters Class}

%include SimpleMC4.lhs

\subsection{Concepts introduced}

\begin{description}
\item[Monads] \emph{This needs either in depth explanation here or at the start \dots}
\end{description}

\subsection{Next Steps}

So far most of these changes are trivial - no major hoops have to be jumped through to get to a nice encapsulated approach. Most of the sorrows are being well taken care of by Haskell as a language. This is all as one would hope. Some things stand out as wanting improvement:

\begin{itemize}
\item It would be nice to have statistics on the calculations
\item Reading of the data line by line is not a nice way of constructing the option data to be priced - reading a data file would be nicer.
\item Computers come with multiple cores these days and Monte Carlo is an embarrassing parallel technique - this should be parallelised.
\end{itemize}

\section{Parsing an Input File}

To keep our code clean and small we should factor out the reading of input parameters. This is done by parsing an input file - or string if done through a pipe.

Haskell comes with a very strong parser (namely Parsec), replacing complicated manipulations with lexers that is necessary in other languages. Below a short parser that will be able to parse the following format:

\subsection{The Input File Format}

\begin{verbatim}
Option {
  Expiry: 1.0
  Payoff: Call at 50.0
}

Fixing {
  Spot: 45.0
  Vol: 0.1
  Rate: 0.1
}

Params {
  Paths: 1000000
}
\end{verbatim}

The different parts of the input file are positional and the order as well - which can be easily changed.

\subsection{Applicative Parsec}

We will be using an applicative extension of Parsec, easily produced through the definition of the following module:

%include ApplicativeParsec.lhs

\subsection{Implementing the Parser}

The definition of the applicative instance for the parser allows to parse the return data types in a concise and readable way - once one gets used to it.

Imagining the data type foo that takes two integers to be defined:

\begin{code}
data Foo = Foo {first :: String, second :: String}
\end{code}

The constructor is a function of the following type:

\begin{code}
Foo :: String -> String -> Foo
\end{code}

Each individual parser give back the parsed value - for example a string:

\begin{code}
parseString :: CharParser st String
parseString = string "foo"
\end{code}

We would now like to parse some text, retain two string values and return these in the data type Foo:

\begin{verbatim}
first: foo
second: notfoo
\end{verbatim}

Without the applicative parser this would look like this:

\begin{code}
parseIt1 = do
  spaces >> string "first:" >> spaces 
  first <- many1 letter
  spaces >> string "second:" >> spaces
  second <- many1 letter
  return (Foo first second)
\end{code}

This can parse our sample input successfully:

\begin{code}
parse parseIt1 "from string" "first: foo \n  second: notfoo"
\end{code}

With ApplicativeParsec we can chain the different parts of the parser with the operator \verb+<*>+ and lift the constructor into it with \verb+<$>+. The chained parsers will be given as an argument chain to the lifted function:

\begin{code}
testParse_a = Foo <$> firstparser <*> secondparser
\end{code}

We might want to intersperse the parsers that return values with our syntax. To chain 2 parsers but only give back the result of the first (or second) as an argument, we use the operators \verb+<*+ and \verb+*>+. Our parser becomes:

\begin{code}
parseIt2 = Foo <$>
  (spaces >> string "first:" >> spaces >> many1 letter) <*>
  (spaces >> string "second:" >> spaces >> many1 letter)
\end{code}

This halves the lines required to define the parser without defining intermediate names for the different parts that are parsed.

\subsubsection{Defining some Data Types}

It would be nice to wrap the different entities we want to parse into data types. We already did that for Option and Payoff. Now here come Fixing and InputParams:

\paragraph{Fixing} \mbox{} \linebreak  \mbox{}

%include Fixing.lhs

\paragraph{InputParams}  \mbox{} \linebreak  \mbox{}

%include InputParams.lhs

\subsubsection{The actual Parser}

%include Parse.lhs

\subsection{Using the Parser}

%include SimpleMC4b.lhs

\section{Gathering Statistics}

\subsection{The Statistics Type Class}

%include Stats.lhs

This type class can now be used by our simple Monte Carlo funtion:

%include SimpleMC5.lhs

\subsection{Convergence Table}

The convergence table implementation looks very similar:

%include ConvTable.lhs

Only the main function needs modification to use this - since the return type changed:

%include SimpleMC6.lhs

\subsection{Concepts introduced}

\begin{description}
\item[Backticks] To make a function act like an operator one can use the backtick syntax:
\begin{code}
fu a b = a + b

let c = fu 1 2
let d = 1 `fu` 2
\end{code}
The expressions for \verb+c+ and \verb+d+ are equivalent.
\item[Forcing evaluation with seq] Haskell as a lazy language does not evaluate expressions until they are needed. This is often advantageous, but can lead to space leaks -- the unevaluated expressions are piling up and use a lot of space. This can be easily found by running the programs with \verb[+RTS -sstderr[ which then prints statistics on the runtime. If the garbage collector uses a high percentage, then there are probably inefficiencies there. The expressions
\begin{code}
a `seq` b
\end{code}
makes sure that the expression \verb+a+ is fully evaluated, before returning the result of expression \verb+b+. So if we want to return a specific type with the type constructor:

\begin{code}
let d = Foo a b
\end{code}

then we can enforce that no unevaluated bits are left with the following code:

\begin{code}
let d = a `seq` b `seq` Foo a b
\end{code}

Did we not do this already with \verb+foldl'+? We kind of did, except that \verb+foldl'+ only evaluates to the head normal form, not the fully evaluated expression. Removing the \verb+`seq`+ out of the code above makes it perform very poorly, with over 70\% of the time spent on garbage collection.

Why does unevaluated code create a space leak and why does this have such an impact on performance? The unevalutated code is big and makes the garbage collector reserve a lot of memory. For this the runtime needs to spend a lot of memory -- and time.
\end{description}


\end{document}