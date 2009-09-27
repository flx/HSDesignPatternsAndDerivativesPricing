We need to include our new module as well:

\begin{code}
import GSL.Random.Gen
import GSL.Random.Dist
import Control.Monad
import Payoff
import Option
\end{code}

Our simple Monte Carlo calculator changes to:

\begin{code}
simpleMC3 :: PayoffClass a => (Option a) -> Double -> Double -> Double -> [Double] 
  -> Double
simpleMC3 op spot vol r sample =
  exp ((-r) * (expiry op)) * sumAll / n
  where
    variance          = vol * vol * (expiry op)
    rootVariance      = sqrt variance
    itoCorr           = (-0.5) * variance
    mSpot             = spot * exp (r * (expiry op) + itoCorr)
    sumAll            = sum $ map sumItem sample
    n                 = (fromIntegral $ length sample)
    sumItem gaussian  = payoff (pay op) randomSpot
      where randomSpot = mSpot * exp (rootVariance * gaussian)
\end{code}

\begin{code}
askForInput statement = putStrLn statement >> (liftM read $ getLine)
\end{code}

Main function doing the input and output:

\begin{code}
main = do
  temp    <- askForInput "Enter option type (1 = Call, other = Put)"
  expiry  <- askForInput "Enter Expiry"
  strike  <- askForInput "Enter strike"
  spot    <- askForInput "Enter spot"
  vol     <- askForInput "Enter vol"
  r       <- askForInput "Enter r"
  n       <- askForInput "Enter number of paths"
  let option = if temp == 1 
               then Option expiry (VOPayoff Call strike) 
               else Option expiry (VOPayoff Put strike)
  rng     <- newRNG mt19937
  randomNums <- replicateM n $ getGaussian rng 1.0
  putStrLn $ show $ simpleMC3 option spot vol r randomNums
\end{code}
