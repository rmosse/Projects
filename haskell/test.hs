module Mytest
	where
import IO

main = do
		file <- readFile "a"
		let sheet = Csvparser.parse file
		map f sheet 
			where f sheet = untrim sheet (length (sheet !! 0))
