Defining a simple type class for gathering stats, together with one instance to get the mean:

\begin{code}
module Stats where
  
class Stats a where
  zero :: a
  dumpOne :: a -> Double -> a
  getRes :: a -> [[Double]]
  
data Mean = Mean {sum :: Double, nPaths :: Int} deriving Show

instance Stats Mean where
  zero = Mean 0 0
  dumpOne (Mean s n) res = newSum `seq` newN `seq` Mean newSum newN where
    newSum  = s + res
    newN    = n + 1
  getRes (Mean s n) = [[s / (fromIntegral n)]]

\end{code}
