module Grid
	where

import Text.Regex.Posix


isGridref:: String ->  Bool
isGridref ref 	| ref =~ "[A-Z][0-9]{6}" = True
			  	| ref =~ "[A-Z][0-9]{4}" = True
			  	| ref =~ "[0-9]{6}" = True
			  	| ref =~ "[0-9]{5}" = True
			  	| ref =~ "[0-9]{4}" = True
				| ref =~ "[A-Z][0-9]{3} \\?\\?" = True
				| ref =~ "[A-Z][0-9]{2} [0-9]{2}" = True
				| ref =~ "[A-Z] [0-9{2} [0-9{2}]" = True
				| otherwise = False


grid_fix "4.0" ref 	| 	 ref =~ "[A-Z][0-9]{6}" = init (init ref)
				 	| 	 ref =~ "[A-Z][0-9]{4}" = ref
				 	| 	 ref =~ "[0-9]{6}" = init (init ref)
				 	|	 ref =~ "[0-9]{5}" = init ref
				  	| 	 ref =~ "[0-9]{4}" = ref
				 	| 	 ref =~ "[A-Z][0-9]{3} \\?\\?" = ref
				 	| 	 ref =~ "[A-Z][0-9]{2} [0-9]{2}" = trim ref 
					| 	 ref =~ "[A-Z] [0-9{2} [0-9{2}]" = trim ref



grid_fix "6.0" ref 	| ref =~ "[A-Z][0-9]{6}" = ref
			  		| ref =~ "[A-Z][0-9]{4}" = ref++"00"
			  		| ref =~ "[0-9]{6}" = ref
			  		| ref =~ "[0-9]{5}" = ref ++ "0"
				  	| ref =~ "[0-9]{4}" = ref++"00"
					| ref =~ "[A-Z][0-9]{3} \\?\\?" = ref
					| ref =~ "[A-Z][0-9]{2} [0-9]{2}" = trim ref
					| ref =~ "[A-Z] [0-9{2} [0-9{2}]" = (trim ref)++"00"


trim :: String -> String
trim [] = []
trim (c:cs) | c == ' ' = trim cs
			| otherwise = c : trim cs   
