module Test
	where
import Data.List
import Lexer

mysplit  [] = []
mysplit  ((Ident column):Ascending:ts) = ((Ident column),Ascending): mysplit ts
mysplit  ((Ident column):Descending:ts) = ((Ident column),Descending): mysplit ts
