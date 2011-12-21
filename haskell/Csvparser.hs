module Csvparser
	where

import Data.List.Split

parse file = init (map del (mylines file))
	where del line = delimit [] line 

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
