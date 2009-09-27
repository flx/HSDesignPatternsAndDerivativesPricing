Plenty of new modules to include:

\begin{code}
import System
import Data.List
import Control.Monad
import Control.Monad.MC
import ApplicativeParsec

import Payoff
import Option
import Parameter
import InputParams
import Fixing
import Parse
\end{code}

Our simple Monte Carlo calculator:

\begin{code}
simpleMC4b :: (PayoffClass a) => (Option a) -> Fixing -> Params -> MC Double
simpleMC4b opt fixing params = do
  xs <- replicateM (numPaths params) $ normal 0.0 1.0
  let rt                = integral (rate fixing) 0 (expiry opt)
      variance          = integralSquare (vol fixing) 0 (expiry opt)
      rootVariance      = sqrt variance
      itoCorr           = (-0.5) * variance
      mSpot             = (spot fixing) * exp (rt + itoCorr)
      sumItem gaussian  = payoff (pay opt) $ mSpot * exp (rootVariance * gaussian)
      sumAll = foldl' (+) 0 $ map sumItem xs
  return (exp (-rt) * sumAll / (fromIntegral $ numPaths params))
\end{code}

Main function doing the input and output is now significantly shorter and nicer:

\begin{code}
main = do
  args <- getArgs
  res <- parseFromFile optionFile (head args)
  let val = case res of
              Right (opt, fixing, params) -> 
                show $ evalMC (simpleMC4b opt fixing params) $ mt19937 0
              Left _ -> "Could not parse file."
  putStrLn val
\end{code}
