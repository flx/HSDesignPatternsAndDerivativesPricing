We need to import the monadic versions of our random number generator:

\begin{code}
import Data.List
import Control.Monad
import Control.Monad.MC
\end{code}

This contains our simple Monte Carlo calculator wrapped in the MC monad:

\begin{code}
simpleMC1b :: Double -> Double -> Double -> Double -> Double -> Int -> MC Double
simpleMC1b expiry strike spot vol r n = do
  xs <- replicateM n $ normal 0.0 1.0
  let variance          = vol * vol * expiry
      rootVariance      = sqrt variance
      itoCorr           = (-0.5) * variance
      mSpot             = spot * exp (r * expiry + itoCorr)
      sumItem gaussian  = if payoff > 0 then payoff else 0
        where payoff = (-strike) + mSpot * exp (rootVariance * gaussian)
      sumAll = foldl' (+) 0 $ map sumItem xs
  return (exp ((-r) * expiry) * sumAll / (fromIntegral n))
\end{code}

\begin{code}
askForInput statement = putStrLn statement >> (liftM read $ getLine)
\end{code}

Main function doing the input and output:

\begin{code}
main = do
  expiry  <- askForInput "Enter Expiry"
  strike  <- askForInput "Enter strike"
  spot    <- askForInput "Enter spot"
  vol     <- askForInput "Enter vol"
  r       <- askForInput "Enter r"
  n       <- askForInput "Enter number of paths"
  let seed = 0
      val  = evalMC (simpleMC1b expiry strike spot vol r n) $ mt19937 seed
  putStrLn $ show val
\end{code}
