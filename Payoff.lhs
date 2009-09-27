Define a module Payoff defining the Payoff type class, requiring the definition of one function, namely payoff - which takes the terminal spot value and returns the payoff.

\begin{code}
module Payoff where
  
class PayoffClass a where
    payoff :: a -> Double -> Double
\end{code}

Then we define the data type for vanilla calls and puts:

\begin{code}
data VanillaOption = Put | Call 
  deriving (Show)

data VOPayoff = VOPayoff { 
            ptype :: VanillaOption, 
            strike :: Double } 
  deriving (Show)
\end{code}

Now we define the instance of the PayoffClass type class - defining the 2 payoffs of vanilla calls and puts:

\begin{code}
instance PayoffClass VOPayoff where
  payoff (VOPayoff Call strike) spot = if spot > strike then spot - strike else 0
  payoff (VOPayoff Put strike) spot = if spot < strike then strike - spot else 0
\end{code}
