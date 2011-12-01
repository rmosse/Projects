module Spread 
	where

import IO
import Lexer

main = prompt 

prompt =  do 
	hSetBuffering stdin LineBuffering
	putStr "$ "
	cmd <- getLine
	print (processcmd cmd)
	prompt 

processcmd cmd = Lexer.scan cmd



--	src <- readFile "map-register.csv"
--	something <- prompt src
--	let something = "I'm a cunt"
--	writeFile "map-register.out" something 
--	main 

