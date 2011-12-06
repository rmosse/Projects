module Date
	where

import Text.Regex.Posix
toDate :: String -> (Int, Int, Int)


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


