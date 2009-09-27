\begin{code}
module Option where  
import Payoff
\end{code}

The Option data type can accommodate any payoff defined in a PayoffClass type class:

\begin{code}
data Option a = Option {
            expiry :: Double,
            pay :: a
          }
  deriving (Show)
\end{code}

One might be tempted to put a type restriction to PayoffClass on the type a - which is not necessary and would force all future functions to have this type restriction as well. As soon as we use the type a in a context where this needs to be a payoff, the type will be inferred there.
