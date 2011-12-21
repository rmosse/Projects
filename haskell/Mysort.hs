module Mysort
	where
import Data.List
import Lexer

mysort :: [[String]] -> [Int] -> [Token]-> [[String]]
mysort sheet (x:xs) (Descending:ds) = sortBy desc sheet
						where 
							desc rowa rowb 
								| length rowa < x || length rowb < x = LT
								| (rowa !! x) < (rowb !! x) = GT
								| (rowa !! x) > (rowb !! x) = LT
								| (rowa !! x) == (rowb !! x) = step xs ds rowa rowb

mysort sheet (x:xs) (Ascending:ds) = sortBy asc sheet
						where 
							asc rowa rowb 
								| length rowa < x || length rowb < x = LT
								| (rowa !! x) > (rowb !! x) = GT
								| (rowa !! x) < (rowb !! x) = LT
								| (rowa !! x) == (rowb !! x) = step xs ds rowa rowb

step _ [] _ _ = LT
step (x:xs) (Descending:ds) rowa rowb  	| (rowa !! x) < (rowb !! x) = GT
										| (rowa !! x) > (rowb !! x) = LT
										| (rowa !! x) == (rowb !! x) = step xs ds rowa rowb

step (x:xs) (Ascending:ds) rowa rowb  	| (rowa !! x) > (rowb !! x) = GT
										| (rowa !! x) < (rowb !! x) = LT
										| (rowa !! x) == (rowb !! x) = step xs ds rowa rowb




wtf (r:rs) = length r : wtf rs
