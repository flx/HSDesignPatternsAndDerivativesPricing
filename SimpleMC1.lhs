We need GSL to get the random number generators and Control.Monad for some monadic operations:

\begin{code}
import Data.List
import GSL.Random.Gen
import GSL.Random.Dist
import Control.Monad
\end{code}

This contains our simple Monte Carlo calculator:

\begin{code}
simpleMC1 :: Double -> Double -> Double -> Double -> Double -> [Double] -> Double
simpleMC1 expiry strike spot vol r sample =
  exp ((-r) * expiry) * sumAll / n
  where
    variance          = vol * vol * expiry
    rootVariance      = sqrt variance
    itoCorr           = (-0.5) * variance
    mSpot             = spot * exp (r * expiry + itoCorr)
    sumAll            = foldl' (+) 0 $ map sumItem sample
    n                 = (fromIntegral $ length sample)
    sumItem gaussian  = if payoff > 0 then payoff else 0
      where payoff = (-strike) + mSpot * exp (rootVariance * gaussian)
\end{code}

Factoring out the questions for input values significantly shortens the main body:

\begin{code}
askForInput statement = putStrLn statement >> (liftM read $ getLine)
\end{code}

Main function doing the input and output:

\begin{code}
main = do
  expiry      <- askForInput "Enter Expiry"
  strike      <- askForInput "Enter strike"
  spot        <- askForInput "Enter spot"
  vol         <- askForInput "Enter vol"
  r           <- askForInput "Enter r"
  n           <- askForInput "Enter number of paths"
  rng         <- newRNG mt19937
  randomNums  <- replicateM n $ getGaussian rng 1.0
  putStrLn $ show $ simpleMC1 expiry strike spot vol r randomNums
\end{code}
