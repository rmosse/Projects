import System.Exit
import IO
import Parser
import Lexer
import Csvparser
import Data.Time.Clock
import Data.Time.Calendar
import Text.Regex.Posix
import Date
import Data.List
import System.Console.Readline
import Data.Maybe
import Grid
import Data.Char
import Mysort

main = prompt [] (False, "console")

prompt:: [[String]] -> (Bool, String) -> IO ()
prompt sheet output =  do 
	hSetBuffering stdin LineBuffering
	maybecmd <- readline "$ "
	let cmd' = (Parser.parse (fromJust maybecmd))
	case cmd' of
		(tok:(Invalid str):ts)	-> invalid' sheet output str
		(Newl:ts) -> prompt sheet output
		(Load:ts) -> load ts output
		(Save:ts) -> save ts sheet output
		(Quit:ts) -> quit
		(Show:ts) -> shower sheet output sheet 
		(Insert:ts)	-> myinsert sheet ts output
		tok 	  -> handle sheet output tok


--IO Functions	
--load
load [Quotedstr filename] output = do
			file <- readFile filename
			let sheet =  Csvparser.parse file
			prompt sheet output
			return ()
--save
save [Quotedstr str] sheet output = do
			writeFile str (Csvparser.toString sheet) 
			prompt sheet output
			return ()			
--show
shower [] output@(output', fname) _ = do
				if output' == True
				then 
					appendFile fname "nothing to show, no file loaded"
				else
					putStrLn "nothing to show, no file loaded"
				prompt [] output

shower sheet output@(output', fname) [] = do
				prompt sheet output

shower sheet output@(output', fname) (l:ls)= do
				println output l
				if output' == True
				then
					appendFile fname ("\n")	
				else
					putStr("\n")
				shower sheet output ls

println output [] = return ()
println output@(output', fname) (w:ws) = do
					if output' == True
					then
						appendFile fname (w ++"|")					
					else
						putStr (w ++"|")
					println output ws				

--quit
quit = do 
			putStrLn "Good bye"
		  	return ()

--Invalid
invalid' sheet output str= do 
				putStrLn str
				prompt sheet output
				return ()


--Insert

myinsert sheet@(r:rs) args output  = myinsert' sheet args (length $ trimend r) output
myinsert' sheet args len output	| checkinput (myinsert'' sheet args) len == (True, len) = inputOK (unzip (myinsert'' sheet args)) sheet output
								| otherwise =  promptinput ( checkinput (myinsert'' sheet args) len) args sheet output
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


promptinput::(Bool,Int) -> [Token] -> [[String]] -> (Bool, String) -> IO ()
promptinput (tval, index) args sheet output | index == -1 = do
											putStrLn "you entered to many values, changes disgarded"
											prompt sheet output
											return ()
									  		| otherwise =   do
											putStr ("please enter a value for column "++ show index++": ")
											val <- getLine
											myinsert sheet (args++[ConditionStr (show index) "=" val]) output
																	
inputOK::([Int],[String]) -> [[String]] -> (Bool, String) -> IO ()
inputOK (inds, vals) sheet@(r:rs) output = do 
								  let vals' = untrim vals (length r)
								  putStrLn "1 new row added"
								  let x = sheet ++ [vals']
								  prompt x output	
								  return ()		



trimend line = trimend' (reverse line)
trimend' line@(x:xs) 	| x == "" = trimend' xs
						| otherwise = reverse line
untrim::[String] -> Int -> [String]
untrim vals len = vals ++ (untrim' (length vals) len)
untrim'::Int-> Int -> [String]
untrim' index len | index == len = []
				  | otherwise = "" : untrim' (index+1) len


sorter::[(Int, String)] -> [(Int,String)]
sorter vals  = sortBy order vals
						where 
							order (inda, vala) (indb, valb)
								| inda > indb = GT
								| inda < indb = LT
								| inda == indb = error ("multiple declarations for column " ++ (show inda))



--processes non IO functions
handle sheet output toks = do
						let (msg, sheet', (output',fname)) = (process sheet (date) output toks)
							where date = (2012,12,11)
						if output' == True						
						then
							appendFile fname msg
						else
							putStr (msg)
						prompt sheet' (output',fname)
						return ()

process:: [[String]] -> (Int, Int, Int) -> (Bool,String) -> [Token] -> (String,[[String]],(Bool,String))
process sheet date output toks =  case toks of							
							((Invalid str):ts)		-> invalid sheet output str
							(tok:(Invalid str):ts)	-> invalid sheet output str
							(Report:ts) 			-> report sheet date output ts
							(Count:ts)    			-> count sheet output ts
							(List:ts)    			-> list sheet output ts
							(Distinct:ts)    		-> distinct sheet output ts							
							(Output:ts)   	 		-> out_put sheet output ts		
							(NoOutput:ts)   		-> nooutput sheet output ts		
							(Help:ts)				-> help sheet output
							(DateFix:ts) 			-> datefix sheet output ts
							(GridFix:ts) 			-> gridfix sheet output ts
							(Reformat:ts)   		-> reformat sheet output ts
							(Sort:ts)  				-> sortf sheet output ts
							(Select:ts) 			-> select sheet output ts
							(Update:ts)				-> update sheet output ts
							(Delete:ts) 			-> deletef sheet output ts
							junk					-> (("invalid") , sheet , output)

--Regular Functions
invalid sheet output msg = (msg, sheet, output)

--report
report:: [[String]] -> (Int,Int,Int) -> (Bool, String) -> [Token] ->  (String ,[[String]] , (Bool, String))
report sheet date output [Registrations] =  (answer, sheet , output)
	where 
		answer = getClub sheet 

report sheet date output [Completions] = (answer , sheet, output)
		where
			answer = foldl (++) "" (map (++"\n")  (getComps date sheet))

getClub :: [[String]] -> [Char]
getClub [] = countreplicas [] 
getClub str = countreplicas (getClub' str)
getClub' [] = []
getClub' (r:rs) =  ((getitem r):(getClub' rs)) 
getitem ::[String] -> [Char]
getitem []  = [] 
getitem (c:cs) = c

countreplicas ::[String] -> [Char]
countreplicas [] = "0\n"
countreplicas (c:cs)= countreplicas' 1 c cs
countreplicas':: Int -> String -> [String] -> [Char]
countreplicas' num last [] = last ++": "++ (show num) ++ "\n"
countreplicas' num last (c:cs) 	| c == last = countreplicas' (num+1) last cs
					 		  	| otherwise =(last ++": "++ (show num) ++ "\n") ++ (countreplicas' 1 c cs)


getComps:: (Int,Int,Int)-> [[String]] -> [String]
getComps date [] =[]
getComps date (c:cs) = getComps' date (getcolumn 0 "Expected Completion Date" c) (getcolumn 0 "Map Name" c) cs

getComps':: (Int,Int,Int)-> Int -> Int -> [[String]] -> [String]
getComps' date i j [] = []
getComps' date i j (r:rows)	| (length r) < i || (length r) < j = []
							| checkcompleted (r !! i) date = (r !! j): getComps' date i j rows
							| otherwise = getComps' date i j rows

getcolumn ::Int -> String-> [String] -> Int
getcolumn i name (c:cs) 		| c == name = i 
								| otherwise = getcolumn (i+1) name cs
checkcompleted:: String -> (Int, Int, Int) -> Bool
checkcompleted date cdate =  do
						let (yyyy,mm,dd) = cdate
						let (day, month, year) = Date.toDate date
						checkcompleted' dd mm yyyy day month year

checkcompleted' d m y cd cm cy 	| y > cy = True
							   	| y < cy = False
								| y == cy && m > cm = True
 								| y == cy && m < cm = False
								| y == cy && m == cm && d > cd = True
								| cy == 0 && cm == 0 && cd == 0 = True
								| otherwise = False


--count
count :: [[String]] -> (Bool, String) -> [Token] -> (String , [[String]], (Bool, String))
count sheet output conds = ((show (length (evalconds sheet conds)))++"\n", sheet, output)

evalconds :: [[String]] -> [Token] -> [Bool]
evalconds [] conds = []
evalconds (r:rs) conds | (evalcell r conds) = True : (evalconds rs conds)
					   | otherwise = (evalconds rs conds)
evalcell :: [String] ->  [Token] -> Bool
evalcell [""] _ = False
evalcell row [] = True
evalcell row ((ConditionStr rownum condition str):cs) 	| ((length row) < ((read rownum)::Int)) = False
														| (row !! ((read rownum)::Int)) =~ str = True && evalcell row cs
														| otherwise = False

--list
list :: [[String]] -> (Bool, String) -> [Token] -> (String , [[String]], (Bool, String))
list sheet output conds = ((foldl (++) ""  (map (++" ") (evalconds' sheet conds))), sheet, output)

evalconds' :: [[String]] -> [Token] -> [String]
evalconds' [] conds = []
evalconds' (r:rs) conds 	| (evalcell' r conds) = (strip r) ++ ["\n"] ++ (evalconds' rs conds)
							| otherwise = (evalconds' rs conds)

evalcell' :: [String] ->  [Token] -> Bool
evalcell' [""] _ = False
evalcell' row [] = True
evalcell' row ((ConditionStr rownum condition str):cs) 	| ((length row) < ((read rownum)::Int)) = False
														| (row !! ((read rownum)::Int)) =~ str = True && evalcell' row cs
														| otherwise = False
																
								

strip [] = []
strip (c:cs) | c /= "" = c : strip cs
			 | otherwise = strip cs



--distinct
distinct :: [[String]] -> (Bool, String) -> [Token] -> (String, [[String]], (Bool, String))
distinct sheet output (args:ts) = (findunique sheet args, sheet, output)
findunique sheet (Quotedstr column) = foldl (++) "" (map (++"\n") (unique column' column'))
	where column' = (getcol (((read column)::Int)) sheet)

getcol ::Int -> [[String]] -> [String]
getcol i [] = []
getcol i (r:rs) | i > ((length r) -1) = [] :  getcol i rs
				| otherwise = (r !! i) : getcol i rs


unique list [] = []				
unique list (c:cs)  | (notElem c (delete c list)) = c : unique list cs
					| otherwise = unique list cs




--output
out_put sheet output [Quotedstr str]	= (("output redirected to "++str++"\n"), sheet, (True,str))

--nooutput
nooutput sheet output args = ("output redirected to console" , sheet, (False,"console"))

--help
help sheet output = ("help", sheet, output)

--datefix

datefix sheet@(r:rs) output args = ("dates updated\n" , datefix' rs args, output)
datefix [] output args = ("no file loaded\n",[],output)
datefix' :: [[String]] -> [Token] -> [[String]] 
datefix' [] _ = []
datefix' (r:rs) args@[(Ident str),(Date format)] = (datefix'' r args ((read str)::Int)) : (datefix' rs args) 

datefix'' :: [String] -> [Token] -> Int -> [String]
datefix'' [] _ _ = []
datefix'' row [(Ident str),(Date format)] index | length row < index = []
										  		| isDate (row !! index) =  (setIndex row index (Date.date_fix format (Date.toDate (row !! index))))
										  		| (row !! index) == "" = ["0/0/0",[]]			
									 	  		| otherwise = error ("Not a Date \"" ++ (row !! index) ++ "\"")
setIndex row index value
    | index < 0 = error "Bad index"
    | otherwise = _setIndex row index value
    where
        _setIndex [] _ _ = error "Bad index"
        _setIndex (_ : row) 0 value = value : row
        _setIndex (x : xs) index value = x : (setIndex xs (index - 1) value)


--gridfix
gridfix sheet@(r:rs) output args = ("Grid references updated\n", gridfix' rs args, output)
gridfix [] output args = ("no file loaded\n",[],output)
gridfix' :: [[String]] -> [Token] -> [[String]] 
gridfix' [] _ = []
gridfix' (r:rs) args@[(Ident str),(Const format)] = (gridfix'' r args ((read str)::Int)) : (gridfix' rs args) 

gridfix'' :: [String] -> [Token] -> Int -> [String]
gridfix'' [] _ _ = []
gridfix'' row [(Ident str),(Const format)] index | length row < index = []
										  		| isGridref (row !! index) =  (setIndex row index (Grid.grid_fix (show format) (row !! index)))
										  		| (row !! index) == "" = ["0/0/0",[]]			
									 	  		| otherwise = ( error ("Not a Gridreference \"" ++ (row !! index) ++ "\""))


reformat sheet output [Ident num, Uppercase] = ("updated column: "++(show num)++"\n", upper ((read num)::Int) sheet, output)
reformat sheet output [Ident num, Lowercase] = ("updated column: "++(show num)++"\n", lower ((read num)::Int) sheet, output)
reformat sheet output [Ident num, Capitalize] = ("updated column: "++(show num)++"\n", caps ((read num)::Int) sheet, output)
reformat sheet output [Ident num, Trim] =  ("updated column: "++(show num)++"\n",trimmer ((read num)::Int) sheet, output)


upper:: Int -> [[String]] -> [[String]]
upper _ [] = []
upper num (r:rs) | length r > num = setIndex r num (map toUpper (r !! num)) : upper num rs 
				 | otherwise = []


lower :: Int -> [[String]] -> [[String]]
lower _ [] = []
lower num (r:rs) | length r > num = setIndex r num (map toLower (r !! num)) : lower num rs 
				 | otherwise = []

caps :: Int -> [[String]] -> [[String]]
caps _ [] = []
caps num (r:rs) | length r > num = setIndex r num value : caps num rs 
				| otherwise = []
				where value | length (r !! num) > 0 = (setIndex (r !! num) 0 value')
							| otherwise = (r !! num)						
					where value' =  (toUpper ((r !! num) !! 0))
								 	
trimmer :: Int -> [[String]] -> [[String]]
trimmer _ [] = []
trimmer num (r:rs) 	| length r > num = setIndex r num (Date.trim (r !! num)) : trimmer num rs 
				 	| otherwise = []

sortf sheet output args = ("sorted\n" , sort' sheet args, output)

sort' sheet args = let (columns,orders) =  unzip (mysplit args) in mysort sheet columns orders

mysplit  [] = []
mysplit  ((Ident column):Ascending:ts) = (((read column)::Int),Ascending): mysplit ts
mysplit  ((Ident column):Descending:ts) = (((read column)::Int),Descending): mysplit ts


select sheet output args = (show args, sheet, output)

update sheet output args@((Const r):c:cs) = ("row "++(show (floor r))++" updated\n", update' sheet args, output)
update' sheet ((Const num):(Ident str):(Ident val):[]) = setIndex sheet (floor num) (setIndex (sheet !! (floor num)) ((read str)::Int) val)




deletef sheet output (Const val:[]) = ("row "++(show (floor val))++" deleted\n", delete' sheet (floor val) , output)



delete' sheet val = delete'' 0 sheet val 
delete'' _ [] _ = []
delete'' index (r:rs) val | val == index = delete'' (index+1) rs val
						  | otherwise = r : delete'' (index+1) rs val





junker sheet output args =(show args, sheet, output)
