\begin{code}
import Control.Monad
import Control.Monad.MC
import Text.Printf
\end{code}

\begin{code}
unitBox :: MC (Double,Double)
unitBox = liftM2 (,) (uniform (-1) 1) 
                     (uniform (-1) 1)
\end{code}

We will need a function to test if a point lies inside the unit circle:

\begin{code}
inUnitCircle :: (Double,Double) -> Bool
inUnitCircle (x,y) = x*x + y*y <= 1
\end{code}

Here is a function to generate n points and count how many fall inside the circle, and then to compute an estimate and standard error for pi based on n samples:

\begin{code}
computePi :: Int -> MC (Double,Double)
computePi n = do
    xs <- replicateM n unitBox
    let m  = length $ filter inUnitCircle xs
        p  = toDouble m / toDouble n
        se = sqrt (p * (1 - p) / toDouble n)
    return (4*p, 4*se)

  where
    toDouble = realToFrac . toInteger
\end{code}

Running the Simulation
To get a value out of the MC monad, we must provide it with a random number generator. To get a Mersenne Twister generator, we use the mt19937 function. Then, evaluate the result with evalMC.

Here’s an example:

\begin{code}
main = let
    n       = 1000000000
    seed    = 0
    (mu,se) = evalMC (computePi n) $ mt19937 seed
    delta   = 2.575*se
    (l,u)   = (mu-delta, mu+delta)
    in do
        printf "Estimate:                   %gn" mu
        printf "99%% Confidence Interval:    (%g,%g)n" l u
\end{code}
