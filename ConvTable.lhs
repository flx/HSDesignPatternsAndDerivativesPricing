Defining the convergence table on any stats type:

\begin{code}
module ConvTable where
  
import Stats

data ConvTable a = ConvTable {
  count :: Int, 
  nextLimit :: Int, 
  currStat :: a,
  resList :: [[Double]] }

instance (Stats a) => (Stats (ConvTable a)) where
  zero = (ConvTable 0 1 zero [])
  dumpOne (ConvTable c l curr xs) s = 
    nc `seq` ncurr `seq` nl `seq` nxs `seq` (ConvTable nc nl ncurr nxs) where
    nc = c + 1
    ncurr = dumpOne curr s
    (nl, nxs) = if nc == l then (l * 2, (head $ getRes ncurr):xs)
                else (l,xs)
  getRes (ConvTable c l curr xs) = 
    if c == l then xs else (head $ getRes curr):xs
\end{code}
