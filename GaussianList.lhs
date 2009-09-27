\begin{code}
import GSL.Random.Gen
import GSL.Random.Dist
import Control.Monad
import System.Random


-- this does not work
effectfulStreamFunc :: RNG -> Double -> IO [IO Double]
effectfulStreamFunc rng sigma = do
    let x = getGaussian rng sigma
    rest <- effectfulStreamFunc rng sigma
    return (x:rest)

-- this does not work
main2 :: IO ()
main2 = do
  rng <- newRNG mt19937
  es <- effectfulStreamFunc rng 1.0
  let ll = take 2 es
  print "success"

-- this works but is inefficient ... and probably a bad generator ...
gaussianRs :: Int -> [Double]
gaussianRs seed = map ugaussianPInv $ randomRs (0.0, 1.0) rng where
  rng = mkStdGen seed

main = do
  print $ show $ take 100000000 $ gaussianRs 1000

-- print $ show $ take 2 es
\end{code}
