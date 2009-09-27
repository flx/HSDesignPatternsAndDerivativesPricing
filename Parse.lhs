\begin{code}
module Parse where
  
import Numeric
import Payoff
import Option
import Fixing
import InputParams
import ApplicativeParsec
\end{code}

Parse all parts of the file:

\begin{code}
optionFile = returnVals <$>  opt <*> fixings <*> numParams <* (spaces >> eof) where 
    returnVals a b c = (a,b,c)
\end{code}

Parse the product first:

\begin{code}
opt = Option <$> 
  (spaces >> string "Option" >> spaces >> string "{" >> p_expiry) <*> 
  p_payoff <* 
  (spaces >> string "}" >> eol)
  
p_expiry = (spaces >> (string "Expiry:") >> spaces) *> (p_number <?> "d1")
    
p_payoff = option <$> 
  (spaces >> (string "Payoff:") >> spaces >> (string "Call" <|> string "Put")) <*>
  (spaces1 >> (string "at") >> spaces1 >> (p_number <?> "d1")) where 
    option str strike | str == "Call" = VOPayoff Call strike
    option str strike | str == "Put"  = VOPayoff Put  strike
\end{code}

Parse our 2 fixings:

\begin{code}
fixings = Fixing <$>
  (spaces >> (string "Fixing") >> spaces >> (string "{") >> p_spot) <*> 
  (spaces >> p_vol) <*> 
  (spaces >> p_rate) <* 
  (spaces >> string "}" >> eol)

p_spot = (spaces >> (string "Spot:") >> spaces) *> (p_number <?> "d1")
    
p_vol = (spaces >> (string "Vol:") >> spaces) *> (p_number <?> "d1")

p_rate = (spaces >> (string "Rate:") >> spaces) *> (p_number <?> "d1")
\end{code}

Parse our parameters:

\begin{code}
numParams = Params <$>
  (spaces >> (string "Params") >> spaces >> (string "{") >> p_paths) <* 
  (spaces >> string "}" >> eol)

p_paths = (spaces >> (string "Paths:") >> spaces) *> (p_integer <?> "d1")
\end{code}

Some general parsers:

\begin{code}
p_number :: CharParser () Double
p_number = do 
  s <- getInput
  case readSigned readFloat s of
    [(n, s')] -> n <$ setInput s'
    _         -> empty

p_integer :: CharParser () Int
p_integer = do 
  s <- getInput
  case readSigned readDec s of
    [(n, s')] -> n <$ setInput s'
    _         -> empty

eol = char '\n'

spaces1 = many1 space
\end{code}
