module Date
	where

import Text.Regex.Posix
import Data.Char
toDate :: String -> (Int, Int, Int)
toDate str@(a:b:c:cs) 	
	| str =~ "^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec|jan|feb|mar|apr|may|jun|jul|aug|sep|sept|oct|nov|dec)-[01][0-9]$"
 =  (0, mon2int([a]++[b]++[c]), (year2int [] cs)) --Mmm-yy
	| str =~ "^[0-9][0-9][0-9][0-9]$"  -- YYYY
 =  (0,0,(year2int [] str))
	| str =~ "^[0-9][0-9][0-9][0-9] or [0-9] \\?$"  -- YYYY or y ?
 =   (0,0,(year2int [] str))
	| str =~ "^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec|jan|feb|mar|apr|may|jun|jul|aug|sep|sept|oct|nov|dec) [0-9][0-9][0-9][0-9]$"  -- YYYY
 =  (0,mon2int ([a]++[b]++[c]),year2int [] cs)
	| str =~ "^[0-9][0-9][0-9][0-9]/([0-9]|[0-1][0-9])$"  --YYYY/Y | YYYY / YY
 =  (0,0,year2int [] str)
	| str =~ "^(January|February|March|April|May|June|July|August|September|October|November|December|january|february|march|april|may|june|july|august|september|october|november|december) [0-9][0-9][0-9][0-9]$"
 =	(0, (month2int str), (year2int [] cs))
	| str =~ "^(spring|summer|autumn|winter|Spring|Summer|Autumn|Winter) [0-9][0-9][0-9][0-9]$" 
 =	(season2day str,season2mon str, year)
	| str =~ "^([0-9]|[012][0-9]|[3][01])-(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec|jan|feb|mar|apr|may|jun|jul|aug|sep|sept|oct|nov|dec)$" 
 = 	(year2int [] str, mon2int str, 0)

	| str =~ "^([0-9]|[012][0-9]|[3][01])-(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec|jan|feb|mar|apr|may|jun|jul|aug|sep|sept|oct|nov|dec)$" 
 = (year2int [] str, mon2int str, 0)
	| str =~ ".*completed.*" 
 = tuplify (slashdateparser [] str)
	| str =~ "^[012][0-9]/[01][0-9]/[12][0-9][0-9][0-9]$" 
 = tuplify (slashdateparser [] str)

	| str =~ "^[3][01]/[01][0-9]/[12][0-9][0-9][0-9]$" 
 = tuplify (slashdateparser [] str)
	| str =~ ""
 = (0,0,9000)

		where year = 	if (season2mon str) == 3
						then (year2int [] str) +1
						else (year2int [] str) 
	
toDate _ = (0,0,9000)

isDate :: String -> Bool
isDate str 	| str =~ "^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec|jan|feb|mar|apr|may|jun|jul|aug|sep|sept|oct|nov|dec)-[01][0-9]$" = True--Mmm-yy
			| str =~ "^[0-9][0-9][0-9][0-9]$" = True  -- YYYY
			| str =~ "^[0-9][0-9][0-9][0-9] or [0-9] \\?$" = True  -- YYYY or y ?
			| str =~ "^[JjFfMmAaSsOoNnDd][AaEePpUuOo][NnBbRrYyLlGgPpVvCc] [0-9][0-9][0-9][0-9]$" = True  -- YYYY
			| str =~ "^[0-9][0-9][0-9][0-9]/([0-9]|[0-1][0-1])$" = True --YYYY/Y | YYYY / YY
			| str =~ "^(January|February|March|April|May|June|July|August|September|October|November|December|january|february|march|april|may|june|july|august|september|october|november|december) [0-9][0-9][0-9][0-9]$" = True
			| str =~ "^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec|jan|feb|mar|apr|may|jun|jul|aug|sep|sept|oct|nov|dec) [0-9][0-9][0-9][0-9]$" = True
			| str =~ "^(spring|summer|autumn|winter|Spring|Summer|Autumn|Winter) [0-9][0-9][0-9][0-9]$" = True
			| str =~ "^([0-9]|[01][0-9])-(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec|jan|feb|mar|apr|may|jun|jul|aug|sep|sept|oct|nov|dec)$" = True
			| str =~ "^([0-9]|[01][0-9])-(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Sept|Oct|Nov|Dec|jan|feb|mar|apr|may|jun|jul|aug|sep|sept|oct|nov|dec)$" = True
			| str =~ ".*completed.*" = True
			| str =~ "^[012][0-9]/[01][0-9]/[12][0-9][0-9][0-9]$" = True
			| str =~ "^[3][01]/[01][0-9]/[12][0-9][0-9][0-9]$" = True


isDate _ = False
mon2int :: String -> Int
mon2int mon = mon2int' (getMon [] mon) 
mon2int' :: String -> Int
mon2int' mon	
			| mon =~ "Jan|jan" = 1
			| mon =~ "Feb|feb" = 2
			| mon =~ "Mar|mar" = 3
			| mon =~ "Apr|apr" = 4
			| mon =~ "May|may" = 5
			| mon =~ "Jun|jun" = 6
			| mon =~ "Jul|jul" = 7
			| mon =~ "Aug|aug" = 8
			| mon =~ "Sep|sep" = 9
			| mon =~ "Oct|oct" = 10
			| mon =~ "Nov|nov" = 11
			| mon =~ "Dec|dec" = 12

month2int :: String -> Int
month2int str = month2int' (getMon [] str) 

month2int' :: String -> Int
month2int' mon 
			| mon =~ "January|january" = 1
			| mon =~ "February|february" = 2
			| mon =~ "March|march" = 3
			| mon =~ "April|april" = 4
			| mon =~ "May|may" = 5
			| mon =~ "June|june" = 6
			| mon =~ "July|july" = 7
			| mon =~ "August|august" = 8
			| mon =~ "September|september" = 9
			| mon =~ "October|october" = 10
			| mon =~ "November|november" = 11
			| mon =~ "December|december" = 12

getMon :: String -> String -> String
getMon buffer [] 		= buffer
getMon buffer (c:cs) 	| c == ' ' = buffer
						| isDigit c = getMon buffer cs 
						| otherwise = getMon (buffer++[c]) cs

year2int :: String -> String -> Int
year2int buffer [] = read buffer :: Int
year2int buffer (c:cs) 	| (length buffer) > 3 = year2int [] cs
						| isDigit c = year2int (buffer++[c]) cs
						| otherwise = year2int buffer cs

season2day str = season2day' (getMon [] str)
season2day' str 
			| str =~ "Spring|spring" = 21
			| str =~ "Summer|summer" = 23
			| str =~ "Autumn|autumn" = 21
			| str =~ "Winter|winter" = 21

season2mon str = season2mon' (getMon [] str)
season2mon' str 
			| str =~ "Spring|spring" = 6
			| str =~ "Summer|summer" = 9
			| str =~ "Autumn|autumn" = 12
			| str =~ "Winter|winter" = 3

slashdateparser::  String -> String -> [String]
slashdateparser  buffer  [] = []
slashdateparser  buffer (c:cs) 	| c == '/' = buffer : (slashdateparser' [] cs) 
							 	| isDigit c = slashdateparser (buffer++[c]) cs
								| otherwise = slashdateparser buffer cs

slashdateparser'::  String -> String -> [String]
slashdateparser' buffer (c:cs) 	| c == '/' = buffer : (slashdateparser'' [] cs) 
							 	| isDigit c = slashdateparser' (buffer++[c]) cs
								| otherwise = slashdateparser' buffer cs

slashdateparser''::  String -> String -> [String]
slashdateparser'' buffer []   = [buffer]
slashdateparser'' buffer (c:cs) | isDigit c = slashdateparser'' (buffer++ [c]) cs
								| otherwise = slashdateparser'' buffer cs

tuplify :: [String] -> (Int,Int,Int)
tuplify [] = (0,0,9000)
tuplify [x,y,z] = ((read x),(read y), (read z))

