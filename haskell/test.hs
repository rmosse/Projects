module Test
	where
import Data.List
import Lexer
import IO

myinsert sheet@(r:rs) args = myinsert' sheet args (length $ trimend r)
myinsert' sheet args len 	| checkinput (myinsert'' sheet args) len == (True, len) = inputOK (unzip (myinsert'' sheet args)) sheet
							| otherwise =  promptinput ( checkinput (myinsert'' sheet args) len) args sheet
myinsert'' sheet [] = []
myinsert'' sheet ((ConditionStr num cond str):ts) = sorter ([(((read num)::Int), str)] ++ (myinsert'' sheet ts))
myinsert'' sheet junk = [(-1, show junk)]

checkinput:: [(Int,String)] -> Int -> (Bool,Int)
checkinput vals len = checkinput' 0 vals len
checkinput' index ((num,val):ts) len	| num == index = checkinput' (index+1) ts len	 
										| num /= index = (False, index)
										| len == index = (True, index)
checkinput' index [] len 	| index > len = (False, -1)
							| len == index = (True, index)
							| otherwise = (False, index)


promptinput::(Bool,Int) -> [Token] -> [[String]]-> IO ()
promptinput (tval, index) args 	sheet | index == -1 = do
											putStrLn "you entered to many values try again"
											--prompt here
											return ()
						  		| otherwise =   do
											putStr ("please enter a value for column "++ show index++": ")
											val <- getLine
											myinsert sheet (args++[ConditionStr (show index) "=" val])
											
																

																						


inputOK::([Int],[String]) -> [[String]] -> IO ()
inputOK (inds, vals) sheet = do 
								putStrLn "1 new row added"
								let x = sheet ++ [vals]
								--prompt here	
								return ()		







trimend line = trimend' (reverse line)
trimend' line@(x:xs) 	| x == "" = trimend' xs
						| otherwise = reverse line

sorter::[(Int, String)] -> [(Int,String)]
sorter vals  = sortBy order vals
						where 
							order (inda, vala) (indb, valb)
								| inda > indb = GT
								| inda < indb = LT
								| inda == indb = error ("multiple declarations for column " ++ (show inda))

