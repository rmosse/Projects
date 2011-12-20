module Test
	where
import Data.Char

mysort':: [[String]] -> Int -> [[String]]
mysort [] _ = []
mysort' (r:rs) index =  mysort' lesser index ++ [r] ++ mysort' greater index
 					where
						lesser = filter (< (r !! index)) rs 
						greater = filter (>= (r !! index)) rs			
