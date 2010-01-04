
{-
Copyright (C) Rajesh Krishnan.
The following code is available to anyone in the GNU Lesser GPL Version 3 license.
-}
	
import System.IO
import Control.Monad
import Control.Monad.List


-- type ListIO = ListT IO

-- In real life, getCust would execute a SQL query to return a list of customers from an RDBMS,
-- but here I am just simulating it using hardcoded return values...
-- Note that the return type is like:  M1 (M2 (a)), where M1 and M2 are monadic type constructors (List is a monad too).
-- In our case IO [String] ==  IO (List String) == (IO . List) String.
-- So List is a monad that is nested inside the outer IO monad, which is precisely what is contained
-- within the ListT wrapper (m [a] contained in the definition of ListT):
-- http://haskell.org/ghc/docs/latest/html/libraries/mtl/src/Control-Monad-List.html#ListT

-- Also note that the bind operator (>>=) for ListT "peels" off 2 layers of monads simultaneously
-- in every step of the monadic generator (<- operator in the do block of enumerate function below).

getCust :: IO [String]
getCust = return ["cust1", "cust2", "cust3", "cust4"]

-- In real life, getCust would execute a SQL query to return a list of orders from an RDBMS for a given customer,
-- but here I am just simulating it using hardcoded return values...
getOrd :: String -> IO [String]
getOrd sCust = case sCust of
	"cust1" -> return ["ord1a", "ord1b"]
	"cust2" -> return ["ord2a", "ord2b", "ord2c"]
	"cust3" -> return ["ord3a", "ord3b", "ord3c", "ord3d"]
	"cust4" -> return []
	
-- In real life, getCust would execute a SQL query to return a list of products from an RDBMS the customer bought
-- for a given purchase order, but here I am just simulating it using hardcoded return values...
getProd :: String -> IO [String]
getProd sOrd = case sOrd of
	"ord1a" -> return ["prod1a1", "prod1a2"]
	"ord1b" -> return ["prodab1"]
	"ord2a" -> return ["prod2a1", "prod2a2", "prod2a3"]
	"ord2b" -> return ["prod2b1", "prod2b2"]
	"ord2c" -> return ["prod2c1"]
	"ord3a" -> return ["prod3a1", "prod3a2", "prod3a3"]
	"ord3b" -> return ["prod3b1", "prod3b2" ]
	"ord3c" -> return ["prod3c1", "prod1a1", "prod2a1"]
	"ord3d" -> return []

-- The actual generation (pulling out using the <- operator) of monadic values out of the List contained inside IO ...
enumerate :: ListT IO ()
enumerate = do
	-- Read the (<-) operator in the following statement as:
	-- extract every customer value contained in the transformed Monad (ListT IO)
	-- by first "peeling off" IO and then List ([]) ...
	sCust			<- ListT  $ getCust
	_				<- liftIO $ putStrLn ("Choosing Customer: " ++ sCust)
	sOrd			<- ListT  $ getOrd sCust
	_				<- liftIO $ putStrLn ("Choosing Customer: " ++ sCust ++ "   and Order: " ++ sOrd)
	sProd			<- ListT  $ getProd sOrd
	_				<- liftIO $ putStrLn ("Choosing Customer: " ++ sCust ++ "   and Order: " ++ sOrd ++ "  and Product: " ++ sProd)
	_				<- liftIO $ putStrLn "-----------------------------------------------------------------"
	_				<- liftIO $ putStrLn ""
	return ()
	
main :: IO ()
main	=	do
	unit			<-	runListT $ enumerate
	return ()
	