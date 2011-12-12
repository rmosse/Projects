module Spread 
	where

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

main = prompt [] (False, "console")

prompt:: [[String]] -> (Bool, String) -> IO ()
prompt sheet output =  do 
	hSetBuffering stdin LineBuffering
	putStr "$ "
	cmd <- getLine
	let cmd' = (Parser.parse cmd)
	case cmd' of
		(tok:(Invalid str):ts)	-> invalid' sheet output str
		(Newl:ts) -> prompt sheet output
		(Load:ts) -> load ts output
		(Save:ts) -> save ts sheet output
		(Quit:ts) -> quit
		(Show:ts) -> shower sheet output sheet 
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
							(Insert:ts)				-> insertf sheet output ts
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
datefix sheet output args = (datefix' sheet args, sheet, output)
datefix' :: [[String]] -> [Token] -> String
datefix' [] _ = []
datefix' (r:rs) args = (datefix'' r args) ++ (datefix' rs args) 

datefix'' :: [String] -> [Token] -> String
datefix'' [] _ = []
datefix'' (c:cs) args 	| isDate c =  c ++"\n"++ (datefix'' cs args)
					 	| otherwise = datefix'' cs args

gridfix sheet output args = (show args, sheet, output)

reformat sheet output args = (show args, sheet, output)

sortf sheet output args = (show args, sheet, output)

--sort' sheet (Descending:ts) =

select sheet output args = (show args, sheet, output)

update sheet output args = (show args, sheet, output)

deletef sheet output args = (show args, sheet, output)

insertf sheet output args = (show args, sheet, output)

junker sheet output args =(show args, sheet, output)
