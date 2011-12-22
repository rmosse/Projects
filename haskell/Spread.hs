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
import Data.List.Split
import System.Console.Readline
import Data.Maybe
import Grid
import Data.Char
import Mysort
import System.Directory
import Data.Time.Clock
import Data.Time.Calendar
import qualified Control.Exception as E
import qualified Control.Exception as E
main = prompt [] (False, "console")

prompt:: [[String]] -> (Bool, String) -> IO ()
prompt sheet output =  do 
	hSetBuffering stdin LineBuffering
	maybecmd <- readline "$ "
	let cmd' = (Parser.parse (fromJust maybecmd))
	case cmd' of
		(tok:(Invalid str):ts)	-> invalid' sheet output str
		(Newl:ts) -> prompt sheet output
		(Load:ts) -> load ts sheet output
		(Save:ts) -> save ts sheet output
		(Quit:ts) -> quit
		(Show:ts) -> shower 0 sheet output sheet 
		(Insert:ts)	-> myinsert sheet ts output
		(Select:ts) -> select sheet output ts
		tok 	  -> handle sheet output tok


--IO Functions	
--load
load [Quotedstr filename] sheet output = do
			file <- readFile filename
			let sheet =  Csvparser.parse file
			putStrLn ((show ((length sheet))) ++ " records loaded")
			prompt sheet output
			return ()
			`catch` \ex -> do
							print ex			
							prompt sheet output
							return ()


--save
save [Quotedstr str] sheet output = do
			let a = sheet
			writeFile str (Csvparser.toString sheet) 
			prompt sheet output
			return ()			
			`catch` \ex -> do
							print ex			
							prompt sheet output
							return ()

--show
shower index [] output@(output', fname) _ = do
				if output' == True
				then 
					appendFile fname "nothing to show, no file loaded"
				else
					putStrLn "nothing to show, no file loaded"
				prompt [] output

shower index sheet output@(output', fname) [] = do
				prompt sheet output

shower index sheet output@(output', fname) (l:ls)= do
				putStr (show index++" ")
				println (getmaxlens sheet) output l
				if output' == True
				then
					appendFile fname ("\n")	
				else
					putStr("\n")
				shower (index+1) sheet output ls

println _ output [] = return ()
println (x:xs) output@(output', fname) (w:ws) = do
					let spaces = findspaces w (x+1)
					if output' == True
					then
						appendFile fname (w ++spaces)					
					else
						putStr (w ++spaces)
					println xs output ws				


findspaces cell n | (length cell) < n = makespaces (n -(length cell))
				  | otherwise = ""

makespaces 0 = ""
makespaces num = " " ++ makespaces (num-1)

getmaxlens sheet = reverse ((getmaxlens' sheet (length (sheet !! 0)-1)))
getmaxlens' sheet (-1) = []
getmaxlens' sheet index = maximum (map (!! index) (init (splitEvery 9 (map length (foldr (++) [""] sheet))))) : getmaxlens' sheet (index-1)
					
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
								  let vals' = untrim (length r) vals 
								  putStrLn "1 new row added"
								  let x = sheet ++ [vals']
								  prompt x output	
								  return ()		



trimend line = trimend' (reverse line)
trimend' line@(x:xs) 	| x == "" = trimend' xs
						| otherwise = reverse line

untrim:: Int -> [String] -> [String]
untrim len vals = vals ++ (untrim' (length vals) len)
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

--select
select:: [[String]] -> (Bool,String) -> [Token] -> IO ()
select [] output args = do 
							putStrLn "No File Loaded"
							prompt [] output
							return ()



select selection output [Ident "all"] = do 
										file <- readFile ".temp" 										
										let restofsheet =  Csvparser.parse file
										let sheet' = (restofsheet ++ selection)
										removeFile ".temp"
										putStrLn ((show (length sheet')) ++ " rows selected")
										prompt sheet' output		
										return ()
										`catch` \ex -> do
											putStrLn ((show (length selection)) ++ " rows selected")
											prompt selection output
											return ()	

select sheet output args  | checknums (parseconds (sheet !! 0) args) = do
											   	let selection = getselection sheet (parseconds (sheet !! 0) args)
												let sheet' = remove 0 selection sheet
												writeFile ".temp" (Csvparser.toString sheet')
												putStrLn ((show (length selection)) ++ " rows selected")
												prompt selection output
												return ()
						  | otherwise = do 
												putStrLn "Invalid Condition"
												prompt sheet output
												return ()	
												

remove:: Int -> [[String]] -> [[String]] -> [[String]]
remove index selection sheet | index == (length selection) = sheet
remove index selection sheet =  do
		 							let sheet' = delete (selection !! index) sheet 
	 								remove (index+1) selection sheet'
					


getselection :: [[String]] -> [Token] -> [[String]]
getselection [] conds = []
getselection (r:rs) conds 	| (getselection' r conds) =  r : (getselection rs conds)
							| otherwise = (getselection rs conds)

getselection' :: [String] ->  [Token] -> Bool
getselection' [""] _ = False
getselection' row [] = True
getselection' row ((ConditionStr colnum condition str):cs) 	| ((length row) < ((read colnum)::Int)) = False
															| (row !! ((read colnum)::Int)) =~ str = True && getselection' row cs
															| otherwise = False
																
								

-- (show args, sheet, output)

--processes non IO functions
handle sheet output toks = do
						--parse the date
						let date' = getCurrentTime >>= return . toGregorian . utctDay
						date <- date'
						let (y',m,d) = date
						let date = (((read (show y'))::Int)	, m, d)					
						do					
									let (msg, sheet', (output',fname)) = (process sheet (date) output toks)
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
							(Help:ts)				-> help sheet output ts
							(DateFix:ts) 			-> datefix sheet output ts
							(GridFix:ts) 			-> gridfix sheet output ts
							(Reformat:ts)   		-> reformat sheet output ts
							(Sort:ts)  				-> sortf sheet output ts						
							(Update:ts)				-> update sheet output ts
							(Delete:ts) 			-> deletef sheet output ts
							junk					-> (("invalid") , sheet , output)

--Regular Functions
invalid sheet output msg = (msg, sheet, output)

--report
report:: [[String]] -> (Int,Int,Int) -> (Bool, String) -> [Token] ->  (String ,[[String]] , (Bool, String))
report [] _ output _ = ("Error: No file Loaded\n",[], output)
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
count [] output _ = ("Error: No file Loaded\n",[], output)
count sheet output conds = count' sheet output (parseconds (sheet !! 0) conds)

count' :: [[String]] -> (Bool, String) -> [Token] -> (String , [[String]], (Bool, String))
count' [] output _ = ("Error: No file Loaded\n",[], output)
count' sheet output conds = ((show (length (evalconds sheet conds)))++"\n", sheet, output)

evalconds :: [[String]] -> [Token] -> [Bool]
evalconds [] conds = []
evalconds (r:rs) conds | (evalcell r conds) = True : (evalconds rs conds)
					   | otherwise = (evalconds rs conds)
evalcell :: [String] ->  [Token] -> Bool
evalcell [""] _ = False
evalcell row [] = True
evalcell row ((ConditionStr rownum condition str):cs) 	| ((length row) < ((read rownum)::Int)) = False
														| ((read rownum)::Int) < 0 = False
														| (row !! ((read rownum)::Int)) =~ str = True && evalcell row cs
														| otherwise = False

parseconds:: [String] -> [Token] -> [Token]
parseconds headings [] = []
parseconds headings conds@((ConditionStr col c v ): cs) | col =~ "[0-9].*" = (ConditionStr (show ((read col)::Int)) c v) : parseconds headings cs
														| elem col headings = (ConditionStr (show (fromJust (elemIndex col headings))) c v) : parseconds headings cs
														| otherwise = [(ConditionStr "-1" c v) ]

parseconds headings conds@[(Quotedstr col)] | col =~ "[0-9].*" = [(Quotedstr (show ((read col)::Int))) ]
										    | elem col headings = [(Quotedstr (show (fromJust (elemIndex col headings)))) ]
										    | otherwise = [(Quotedstr "-1")]

parseconds headings conds@((Ident col):ord:ts) 	| col =~ "[0-9].*" = (Ident (show ((read col)::Int))): ord: parseconds headings ts
										    	| elem col headings = (Ident (show (fromJust (elemIndex col headings)))):ord : parseconds headings ts
										    	| otherwise = [(Ident "-1"),ord]

parseconds headings conds@[(Ident col)] 	| col =~ "[0-9].*" = [(Ident (show ((read col)::Int)))]
										    | elem col headings = [(Ident (show (fromJust (elemIndex col headings)))) ]
										    | otherwise = [(Ident "-1")]

parseconds headings conds@((Const num):(Ident col):(Quotedstr val):[]) 		| col =~ "[0-9].*" = conds
															    		| elem col headings = ((Const num):(Ident (show (fromJust (elemIndex col headings)))):(Quotedstr val):[])  
										    							| otherwise = ((Const num):(Ident "-1"):(Quotedstr val):[])

--list
list [] output conds = ("Error: No file Loaded\n",[], output)
list sheet output conds = list' sheet output (parseconds (sheet !! 0) conds)
							

list' :: [[String]] -> (Bool, String) -> [Token] -> (String , [[String]], (Bool, String))
list' sheet output conds = ((foldl (++) ""  (map (++" ") (evalconds' sheet conds))), sheet, output)

evalconds' :: [[String]] -> [Token] -> [String]
evalconds' [] conds = []
evalconds' (r:rs) conds 	| (evalcell' r conds) = (strip r) ++ ["\n"] ++ (evalconds' rs conds)
							| otherwise = (evalconds' rs conds)

evalcell' :: [String] ->  [Token] -> Bool
evalcell' [""] _ = False
evalcell' row [] = True
evalcell' row ((ConditionStr rownum condition str):cs) 	| ((length row) < ((read rownum)::Int)) = False
														| ((read rownum)::Int) < 0 = False
														| (row !! ((read rownum)::Int)) =~ str = True && evalcell' row cs
														| otherwise = False
																
								

strip [] = []
strip (c:cs) | c /= "" = c : strip cs
			 | otherwise = strip cs



--distinct
distinct [] output args = ("Error: No file Loaded\n",[], output)
distinct sheet output args = distinct' sheet output (parseconds (sheet !! 0) args)


distinct' :: [[String]] -> (Bool, String) -> [Token] -> (String, [[String]], (Bool, String))
distinct' sheet output ((Quotedstr column):ts) 	| ((read column)::Int) < 0 = ("Invalid column\n",sheet,output)
												| ((read column)::Int) > length (sheet !! 0) = ("Invalid column\n",sheet,output)

distinct' sheet output (args:ts) = (findunique sheet args, sheet, output)


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

--datefix
datefix [] output args = ("no file loaded\n",[],output)
datefix sheet output (a:as) = datefix2 sheet output ((parseconds (sheet !! 0) [a])++ as) 

datefix2 sheet@(r:rs) output args@((Ident str):as) | ((read str)::Int) > 0 =  ("dates updated\n" , (r : datefix' rs args), output)
												   | otherwise = ("Invalid Column\n" , sheet , output)
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
gridfix [] output args = ("No File Loaded\n", [] , output)
gridfix sheet@(r:rs) output (a:as) = gridfix2 sheet output ((parseconds (sheet !! 0) [a]) ++ as) 
gridfix2 sheet@(r:rs) output args@((Ident str):as) 	| ((read str)::Int) > 0 = ("Grid references updated\n", r: gridfix' rs args, output)
												  	| otherwise = ("Invalid Column \n" , sheet , output)
gridfix' :: [[String]] -> [Token] -> [[String]] 
gridfix' [] _ = []
gridfix' (r:rs) args@[(Ident str),(Const format)] = (gridfix'' r args ((read str)::Int)) : (gridfix' rs args) 

gridfix'' :: [String] -> [Token] -> Int -> [String]
gridfix'' [] _ _ = []
gridfix'' row [(Ident str),(Const format)] index | length row < index = []
										  		| isGridref (row !! index) =  (setIndex row index (Grid.grid_fix (show format) (row !! index)))
										  		| (row !! index) == "" = ["0/0/0",[]]			
									 	  		| otherwise = ( error ("Not a Gridreference \"" ++ (row !! index) ++ "\""))


reformat sheet output (a:as) = reformat2 sheet output ((parseconds (sheet !! 0) [a]) ++ as) 
reformat2 sheet output ((Ident num):as) | ((read num )::Int) < 0 = (("invalid column"++num++"\n"), sheet, output)														
reformat2 sheet output [Ident num, Uppercase] = ("updated column: "++(show num)++"\n", upper ((read num)::Int) sheet, output)
reformat2 sheet output [Ident num, Lowercase] = ("updated column: "++(show num)++"\n", lower ((read num)::Int) sheet, output)
reformat2 sheet output [Ident num, Capitalize] = ("updated column: "++(show num)++"\n", caps ((read num)::Int) sheet, output)
reformat2 sheet output [Ident num, Trim] =  ("updated column: "++(show num)++"\n",trimmer ((read num)::Int) sheet, output)


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

--sort
sortf sheet output args  | checknums (parseconds (sheet !! 0) args) = sortf' sheet output (parseconds (sheet !! 0) args)
						 | otherwise = ("Invalid column\n" , sheet, output)	
	
sortf' sheet output args = ("sorted\n" , sort' sheet args, output)
sort' sheet args = let (columns,orders) =  unzip (mysplit args) in mysort sheet columns orders


checknums:: [Token] -> Bool 
checknums [] = True
checknums toks@((Ident num):ord:ts) | ((read num ):: Int) < 0 = False
								    | otherwise = checknums ts
								 		
checknums toks@((ConditionStr num _ _ ):ts) 	| ((read num ):: Int) < 0 = False
								    			| otherwise = checknums ts	

mysplit  [] = []
mysplit  ((Ident column):Ascending:ts) = (((read column)::Int),Ascending): mysplit ts
mysplit  ((Ident column):Descending:ts) = (((read column)::Int),Descending): mysplit ts

--update
update [] output args = ("No File Loaded\n",[],output)
update sheet output args  = update2 sheet output (parseconds (sheet !! 0) args)
update2 sheet output args@((Const r):(Ident c):cs) | ((read c ) :: Int) < 0 = ("Invalid cell\n", sheet, output )	
												   | ((read c ) :: Int) > ((length (sheet !! 0))-1)  = ("Invalid cell\n", sheet, output )	
												   | floor r > (length sheet)  = ("Invalid cell\n", sheet, output )		
												   | otherwise = ("row "++(show (floor r))++" updated\n", update' sheet args, output)		

update' sheet ((Const num):(Ident str):(Quotedstr val):[]) = setIndex sheet (floor num) (setIndex (sheet !! (floor num)) ((read str)::Int) val)



--delete
deletef sheet output (Const val:[]) | (floor val) < 0 = ("Invalid row\n",sheet,output)
									| (floor val) > length sheet = ("Invalid row\n",sheet,output)
									| otherwise = ("row "++(show (floor val))++" deleted\n", delete' sheet (floor val) , output)



delete' sheet val = delete'' 0 sheet val 
delete'' _ [] _ = []
delete'' index (r:rs) val | val == index = delete'' (index+1) rs val
						  | otherwise = r : delete'' (index+1) rs val



--help

help sheet output tok = case tok of							
							(Report:ts) 			-> (, sheet, output)
							(Count:ts)    			-> ("count ...<condition>...\n condition: <column><condition operator><quotedstring>\ncounts number of rows that meet the given conditions", sheet, output)
							(List:ts)    			-> ("List ...<condition>...\n condition: <column><condition operator><quotedstring>\n lists rows that meet the given conditions", sheet, output)
							(Distinct:ts)    		-> (, sheet, output)							
							(Output:ts)   	 		-> ("usage: set output <filename>\n redirect output to a file", sheet, output)		
							(NoOutput:ts)   		-> ("usage: nooutput\nturn off output redirection", sheet, output)		
							(Help:ts)				-> (, sheet, output)
							(DateFix:ts) 			-> (, sheet, output)
							(GridFix:ts) 			-> (, sheet, output)
							(Reformat:ts)   		-> (, sheet, output)
							(Sort:ts)  				-> (, sheet, output)						
							(Update:ts)				-> (, sheet, output)
							(Delete:ts) 			-> (, sheet, output)
							(Load:ts) 				-> ("usage: load <filename>\nloads a file from disk", sheet, output)
							(Save:ts)				-> ("usage: save <filename>\nsaves a file to disk", sheet, output)
							(Quit:ts) 				-> (, sheet, output)
							(Show:ts) 				-> (, sheet, output) 
							(Insert:ts)				-> (, sheet, output)
							(Select:ts) 			-> (, sheet, output)
						



junker sheet output args =(show args, sheet, output)
