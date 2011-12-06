module Spread 
	where

import System.Exit
import IO
import Parser
import Lexer
import Csvparser
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
						let (msg, sheet', (output',fname)) = (process sheet output toks)
						if output' == True						
						then
							appendFile fname msg
						else
							putStr (msg)
						prompt sheet' (output',fname)
						return ()

process:: [[String]] -> (Bool,String) -> [Token] -> (String,[[String]],(Bool,String))
process sheet output toks =  case toks of							
							((Invalid str):ts)		-> invalid sheet output str
							(tok:(Invalid str):ts)	-> invalid sheet output str
							(Report:ts) 			-> report sheet  output ts
							(Count:ts)    			-> count sheet output ts
							(List:ts)    			-> list sheet output ts
							(Distinct:ts)    		-> distinct sheet output ts							
							(Output:ts)   	 		-> out_put sheet output ts		
							(NoOutput:ts)   		-> nooutput sheet output ts		
							(Help:ts)				-> help sheet output
							(DateFix:ts) 			-> datefix sheet output ts
							(GridFix:ts) 			-> gridfix sheet output ts
							(Reformat:ts)   		-> reformat sheet output ts
							(Sort:ts)  				-> sort sheet output ts
							(Select:ts) 			-> select sheet output ts
							(Update:ts)				-> update sheet output ts
							(Delete:ts) 			-> delete sheet output ts
							(Insert:ts)				-> insert sheet output ts
							junk					-> (("invalid") , sheet , output)

--Regular Functions
invalid sheet output msg = (msg, sheet, output)

--report
report sheet output [Registrations] =  (answer, sheet , output)
	where 
		answer = getClub sheet 

report sheet output [Completions] = (answer , sheet, output)
		where
			answer = "nyi\n"

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

--count
count sheet output args = (show args ,sheet, output)

--list
list sheet output args = (show args, sheet, output)

--distinct
distinct sheet output args = (show args, sheet, output)

--output
out_put sheet output [Quotedstr str]	= (("output redirected to "++str++"\n"), sheet, (True,str))

--nooutput
nooutput sheet output args = ("output redirected to console" , sheet, (False,"console"))

--help
help sheet output = ("help", sheet, output)

--datefix
datefix sheet output args = (show args, sheet, output)


gridfix sheet output args = (show args, sheet, output)

reformat sheet output args = (show args, sheet, output)

sort sheet output args = (show args, sheet, output)

select sheet output args = (show args, sheet, output)

update sheet output args = (show args, sheet, output)

delete sheet output args = (show args, sheet, output)

insert sheet output args = (show args, sheet, output)

junker sheet output args =(show args, sheet, output)
