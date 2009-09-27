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

Defining a simple data type for gathering the mean:

\begin{code}
class Stats a where
  zero :: a
  dumpOne :: a -> Double -> a
  getRes :: a -> Double
  
data Mean = Mean {sum :: Double, nPaths :: Int} deriving Show

instance Stats Mean where
  zero = Mean 0 0
  dumpOne (Mean s n) res = newSum `seq` newN `seq` Mean newSum newN where
    newSum  = s + res
    newN    = n + 1
  getRes (Mean s n) = s / (fromIntegral n)

mapred' f z0 xs0 = lgo z0 xs0
    where lgo z []     = z
          lgo z (x:xs) = let z' = dumpOne z (f x) in z' `seq` lgo z' xs
\end{code}

Our simple Monte Carlo calculator:

\begin{code}
simpleMC5 :: (PayoffClass a, Stats c) => (Option a) -> Fixing -> Params -> MC c
simpleMC5 opt fixing params = do
  xs <- replicateM (numPaths params) $ normal 0.0 1.0
  let rt                = integral (rate fixing) 0 (expiry opt)
      variance          = integralSquare (vol fixing) 0 (expiry opt)
      rootVariance      = sqrt variance
      itoCorr           = (-0.5) * variance
      mSpot             = (spot fixing) * exp (rt + itoCorr)
      sumItem gaussian  = exp (-rt) * (payoff (pay opt) $ mSpot * exp (rootVariance * gaussian))
      sumAll            = foldl' dumpOne zero $ map sumItem xs
  return sumAll
\end{code}

Main function doing the input and output is now significantly shorter and nicer:

\begin{code}
main = do
  args <- getArgs
  res <- parseFromFile optionFile (head args)
  let val = case res of
              Right (opt, fixing, params) -> 
                show $ getRes $ evalMC ((simpleMC5 opt fixing params) :: MC Mean) $ mt19937 0
              Left _ -> "Could not parse file."
  putStrLn val
\end{code}
