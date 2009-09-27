\begin{code}
module Fixing where  
\end{code}

Simple data type for our two fixings.

\begin{code}
data Fixing = Fixing {
            spot :: Double,
            vol :: Double,
            rate :: Double
          }
  deriving (Show)
\end{code}
