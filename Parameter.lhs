We need a couple of functions in this class:

\begin{code}
module Parameter where
  
class Parameter a where
  integral :: a -> Double -> Double -> Double
  integralSquare :: a -> Double -> Double -> Double
  mean :: a -> Double -> Double -> Double
  mean param t1 t2 = (integral param t1 t2) / (t2 - t1)
  rootMeanSquare :: a -> Double -> Double -> Double
  rootMeanSquare param t1 t2 = (integralSquare param t1 t2) / (t2 - t1)
\end{code}

And our instance for a constand Double:

\begin{code}
instance Parameter Double where
  integral param t1 t2 = param * (t2 - t1)
  integralSquare param t1 t2 = param * param * (t2 - t1)
\end{code}