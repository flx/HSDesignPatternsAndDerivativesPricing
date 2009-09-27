We need to include our new module as well:

\begin{code}
import Data.List
import Control.Monad
import Control.Monad.MC
import Payoff
import Option
import Parameter
\end{code}

Our simple Monte Carlo calculator changes to:

\begin{code}
simpleMC4 :: (PayoffClass a, Parameter b) => (Option a) -> Double -> b -> b -> Int 
             -> MC Double
simpleMC4 op spot vol r n = do
  xs <- replicateM n $ normal 0.0 1.0
  let rt                = integral r 0 (expiry op)
      variance          = integralSquare vol 0 (expiry op)
      rootVariance      = sqrt variance
      itoCorr           = (-0.5) * variance
      mSpot             = spot * exp (rt + itoCorr)
      sumItem gaussian  = payoff (pay op) $ mSpot * exp (rootVariance * gaussian)
      sumAll = foldl' (+) 0 $ map sumItem xs
  return (exp (-rt) * sumAll / (fromIntegral n))
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
  vol     <- (askForInput "Enter vol") :: IO Double
  r       <- askForInput "Enter r"
  n       <- askForInput "Enter number of paths"
  let option = if temp == 1 
               then Option expiry (VOPayoff Call strike) 
               else Option expiry (VOPayoff Put strike)
  let seed = 0
      val  = evalMC (simpleMC4 option spot vol r n) $ mt19937 seed
  putStrLn $ show val
\end{code}
