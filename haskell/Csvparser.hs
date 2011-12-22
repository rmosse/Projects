module Csvparser
	where

import Data.List.Split

parse file = (init (removeblanks  (map del (mylines file))))
	where del line = (delimit [] line )

mylines string = splitOn "\r" string

delimit:: String -> String ->  [String]
delimit buffer [] = [buffer]
delimit buffer (c:cs) 	| c == '"'  = (delimit ('"':buffer') continue)
						| c == ','	= buffer: delimit [] cs
						| otherwise = delimit (buffer++[c]) cs					
							where (buffer', continue) = delimit' buffer cs



delimit' buffer (c:cs)  | c == '"' = (buffer++['"'], cs)
						| otherwise = delimit' (buffer++[c]) cs

toString:: [[String]] -> String
toString  [] = ""
toString  (l:ls) = (toString' l) ++ (toString ls)

toString':: [String] -> String
toString' [] = "\r"
toString' [w] = w ++ toString' []
toString' (w:ws) = w ++[',']++ (toString' ws)

removeblanks sheet@(r:rs) = removeblanks' (map trim sheet) 
removeblanks' [] = []
removeblanks' (r:rs) | isblank r = removeblanks' rs
				     | otherwise = r : removeblanks' rs

isblank [] = True
isblank (c:cs) | c == "" = isblank cs
			   | otherwise = False			   	

trim line = trim' (reverse line)
trim' [] = []
trim' line@(x:xs) 	| length xs == 8 = reverse line
					| x == "" = trim' xs
					| otherwise = reverse line
