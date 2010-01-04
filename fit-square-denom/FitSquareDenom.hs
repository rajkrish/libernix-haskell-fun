{-
Copyright (C) Rajesh Krishnan.
The following code is available to anyone in the GNU Lesser GPL Version 3 license.

This example tries to fit a given amount N in denominations that are squares of integers.

Example:
With GHC installed run as follows:

$ runghc FitSquareDenom.hs  -v 10

would produce the following output:
1. 10 cents = (10 coins of 1 cent)
2. 10 cents = (6 coins of 1 cent) + (1 coin of 4 cents)
3. 10 cents = (2 coins of 1 cent) + (2 coins of 4 cents)
4. 10 cents = (1 coin of 1 cent) + (1 coin of 9 cents)

Total number of results = 4

Alternatively you can specify -q for quiet mode, in place of -v on the command line.

Compiling using GHC:
$  ghc --make -Wall -O3 -o FitSquareDenom FitSquareDenom.hs

Would produce the binary FitSquareDenom .


-}

module Main where

	-- ----------------------------------------------------------------------------------
	-- import declarations
	import System.Environment		(getArgs, getProgName)
	import System.IO				(putStrLn)
	import Control.Monad			(when, mzero, guard, mapM_, when)
	import Data.List				(lines, reverse, filter, zip)
	import Data.Array				(Array (..), array, (!))
	import System.IO.Unsafe			(unsafePerformIO)
	import Text.Printf				(printf)
	import Data.Either				(Either (..))
	import Data.Tree				(unfoldTree, Tree(..))
	
	-- ----------------------------------------------------------------------------------
	-- type synonym declarations ...
	type	Amount					=	Int
	type	Denom					=	Int
	type	ArraySqrt				=	Array Denom Int
	type	Count					=	Int
	type	DenomCount				=	(Denom, Count)
	
	-- ----------------------------------------------------------------------------------
	-- data type declarations ...
	data 	DenomValue				=	DenomValue
										{	denomCount	::	DenomCount,
											denomAmount	::	Amount,
											denomList	::	[Denom]
										}
										deriving (Eq)
	
	-- ----------------------------------------------------------------------------------
	--	interface implementations ...
	instance Show DenomValue	where
		show dv										=	printf	"%s  %d  %s"
																(show $ denomCount dv)
																(denomAmount dv)
																(show $ denomList dv)
	
	-- ----------------------------------------------------------------------------------
	-- isqrt - computes the integer square root of a given number
	-- got from: http://www.codecodex.com/wiki/index.php?title=Calculate_an_integer_square_root
	isqrt							::	Int -> Int
	isqrt	n						=	if (n <= 0) then 0 else if (n == 1) then 1 else isqrtPosCorrected n
		where
		isqrtPosCorrected			::	Int -> Int
		isqrtPosCorrected	n		=	if nEst * nEst > n then (nEst - 1) else nEst
			where nEst				= 	isqrtPosEstimated n
		isqrtPosEstimated			::	Int -> Int
		isqrtPosEstimated n			=	div (nEst2 + (div n nEst2)) 2
			where
			nEst2					=	isqrt (n - 1)

	-- ----------------------------------------------------------------------------------
	-- cliArg - handle command line arguments. Returns (Verbosity, Amount) 
	cliArg							::	IO (Bool, Int)
	cliArg							=	do
		lstArg						<-	getArgs
		(bVerbose, nAmount)			<-	if (isValidArg lstArg) then return (toRet lstArg) else usage
		return (bVerbose, nAmount)
			where
			isValidArg				::	[String] -> Bool
			isValidArg lstArg		=	(length lstArg == 2) && (elem (lstArg !! 0) ["-q", "-v"] )
			toRet					::	[String] -> (Bool, Int)
			toRet lstArg			=	((lstArg !! 0 == "-v"), read $ lstArg !! 1)
			usage					::	IO (Bool, Int)
			usage					=	do
				sPN					<-	getProgName
				ret					<-	fail $ msg sPN
				return ret
					where
					listMsg sPN		=	[	"\nUSAGE:  " ++ sPN ++ " [-q|-v]  number",
											"   -q        - quiet",
											"   -v        - verbose (print results)",
											"   number    - a positive integer argument"
										]
					msg sPN			=	unlines $ listMsg sPN
	
	-- ----------------------------------------------------------------------------------
	--	getAsqt - returns a list of integral square roots in an array: a[i] = trunc(sqrt(i))
	getAsqt							::	Amount -> ArraySqrt
	getAsqt nAmount					=	array (1, nAmount) [(i, isqrt i) | i <- [1 .. nAmount] ] -- initialization of array  using list comprehension

	-- ----------------------------------------------------------------------------------
	-- getDenoms - returns a list of denominations that could be used for representing
	-- the specified amount, in the the decreasing order of their values
	getDenoms						::	ArraySqrt -> Amount -> [Denom]
	getDenoms	_	0				=	[]  -- boundary condition
	getDenoms asqt nAmount			=	reverse $ map (^2) lstDenomAsc
		where
		lstDenomAsc					=	[1 .. (asqt ! nAmount) + 1] -- adding 1 to get the next higher denomination also (for "head" below)
		
	-- ----------------------------------------------------------------------------------
	--	unfoldDenom - 	helper function to create a tree of denominations ...
	--	the signature of this function should match what the unfoldTree method in Data.Tree requires
	unfoldDenom										::	DenomValue -> (DenomValue, [DenomValue])
	unfoldDenom dv									=	(dv, dvNext nAmount lstDenom)
		where
		-- (nDenom, nCount)							=	denomCount dv
		nAmount										=	denomAmount dv
		lstDenom									=	denomList dv
		dvNext										::	Amount -> [Denom] -> [DenomValue]
		dvNext 0 	_								=	[]			-- boundary condition
		dvNext _ 	[]								=	[]			-- boundary condition
		dvNext nAmount (nDenomNext:lstDenomNext)	=	do			--	monadic notation for lists
			let	nCountMax							=	div nAmount nDenomNext
			nCountNext								<-	[0 .. nCountMax]
			let nAmountNext							=	nAmount - nDenomNext * nCountNext
			let dvn									=	DenomValue (nDenomNext, nCountNext) nAmountNext lstDenomNext
			return dvn

	-- ----------------------------------------------------------------------------------
	--	sequenceTree - create a list of all the scans for a tree
	sequenceTree									::	[a] -> (Tree a) -> [[a]]
	sequenceTree lstStart treeStart					=	case (subForest treeStart) of
		[]											->	do			--	monadic notation for lists
			return lstCurrent
		otherwise									->	do			--	monadic notation for lists
			treeNext								<-	subForest treeStart
			lstNext									<-	sequenceTree lstCurrent treeNext
			return lstNext
		where
			lstCurrent						=	[rootLabel treeStart] ++ lstStart
		
	-- ----------------------------------------------------------------------------------
	-- main - main execution program
	main							:: IO ()
	main							=	do
		(bVerbose, nAmount)			<-	cliArg
		let asqt                 	= 	array (1, nAmount) [(i, isqrt i) | i <- [1 .. nAmount] ]
		let	lstDenom				=	getDenoms asqt nAmount
		let	dvSeed					=	DenomValue ((head lstDenom), 0) nAmount (tail lstDenom)
		let trDenom					=	unfoldTree unfoldDenom dvSeed
		-- _							<-	putStrLn (show trDenom)
		-- _							<-	putStrLn "------------------------------------------------"
		let lstSeq					=	sequenceTree [] trDenom
		let lstSeqFiltered			=	filter filterDvList lstSeq
		-- _							<-	mapM_ (putStrLn . show . filter filterDv) lstSeqFiltered
		let lstSeqStr				=	display nAmount lstSeqFiltered
		_							<-	when bVerbose $ mapM_ putStr lstSeqStr
		_							<-	putStrLn ""
		_							<-	putStrLn (printf "Total number of results = %d\n" (length lstSeqFiltered))
		return ()					--	unit
		where
			filterDvList						::	[DenomValue] -> Bool
			filterDvList []						=	False
			filterDvList (dv:lstDv)				=	if (denomAmount dv == 0) then True else False
			filterDv							::	DenomValue	->	Bool
			filterDv dv							=	(snd . denomCount) dv > 0
			display								::	Amount -> [[DenomValue]] -> [String]
			plural 								::	Int -> String
			plural n								=	if (n > 1) then "s" else ""
			display nAmount lstlstDv			=	do
				(nIndexDvList, lstDv)			<-	zip ([1 .. ] :: [Int]) lstlstDv
				let	lstDvFiltered				=	filter filterDv lstDv
				(nIndexDv, dv)					<-	zip ([1 .. ]  :: [Int]) lstDvFiltered
				let	sHeaderTemp					=	printf "%d. %d cent%s = " nIndexDvList nAmount (plural nAmount)
				let sHeader						=	if (nIndexDv == 1) then sHeaderTemp else ""
				let nDenomTemp					=	fst $ denomCount dv
				let nCountTemp					=	snd $ denomCount dv
				let sDv							=	printf "(%d coin%s of %d cent%s) " nCountTemp (plural nCountTemp) nDenomTemp (plural nDenomTemp)
				let sNewline					=	if (nIndexDv == length lstDvFiltered) then "\n" else "+ "
				let sRet						=	sHeader ++ sDv ++ sNewline
				return sRet
				
	