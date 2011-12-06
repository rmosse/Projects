module Parser

	where

import Lexer
import Data.Char

parse cmd = parseExpr (Lexer.scan cmd)

parseExpr:: [Token] -> [Token]
parseExpr [] = [Newl]
parseExpr (Load:ts) =  Load: fnameparser "usage: load <filename>" ts 
parseExpr (Save:ts) =  Save: fnameparser "usage: save <filename>" ts
parseExpr (Report:ts) = reportparser ts
parseExpr (Count:ts) = Count : (condparser "count ...<condition>...\n condition: <column><condition operator><quotedstring>" [] ts )
parseExpr (List:ts) = List : (condparser "List ...<condition>...\n condition: <column><condition operator><quotedstring>"[] ts )
parseExpr (Distinct:ts) = Distinct: distinctparser ts
parseExpr (Set:Output:ts) = Output: fnameparser "usage: set output <filename>" ts
parseExpr [NoOutput] = [NoOutput]
parseExpr [Show] = [Show]
parseExpr (DateFix:ts) = DateFix: datefixparser ts 	
parseExpr (GridFix:ts) = GridFix: gridfixparser ts
parseExpr (Reformat:ts) = Reformat: reformatparser ts
parseExpr (Sort:ts) = Sort: sortparser ts
parseExpr (Select:ts) = Select: selectparser ts
parseExpr (Update:ts) = Update: updateparser ts
parseExpr (Delete:ts) = Delete: deleteparser ts
parseExpr (Insert:ts) = Insert: insertparser ts
parseExpr (Help:ts) = [Help]
parseExpr (Quit:ts) = [Quit]
parseExpr junk = junkparser ("Invalid command") []

--load
fnameparser usage [(Quotedstr str)] = [(Quotedstr str)]
fnameparser usage [(Ident str)] = [(Quotedstr str)]
fnameparser usage junk = junkparser usage junk

--report
reportparser [Registrations] = [Report,Registrations]
reportparser [Completions] = [Report,Completions]
reportparser junk = junkparser "usage: report <registrations | completions>" junk

--count
condparser:: String -> [Token] -> [Token] -> [Token]
condparser tipe sofar ((Quotedstr name):(Condition cond):(Globstr str):ts) = [ConditionStr name cond str]++(condparser tipe [] ts)
condparser tipe sofar ((Quotedstr name):(Condition cond):(Quotedstr str):ts) = [ConditionStr name cond str]++(condparser tipe [] ts)
condparser tipe sofar ((Ident name):(Condition cond):(Globstr str):ts) = [ConditionStr name cond str]++(condparser tipe [] ts)
condparser tipe sofar ((Ident name):(Condition cond):(Quotedstr str):ts) = [ConditionStr name cond str]++(condparser tipe [] ts)
condparser tipe sofar ((Quotedstr name):(Condition cond):(Ident str):ts) = [ConditionStr name cond str]++(condparser tipe [] ts)
condparser tipe sofar ((Ident name):(Condition cond):(Ident str):ts) = [ConditionStr name cond str]++(condparser tipe [] ts)
condparser tipe sofar [] = sofar
condparser tipe sofar junk = junkparser ("usage: "++tipe) junk

--distinct
distinctparser tok@[Ident str] = tok
distinctparser tok@[Quotedstr str] = tok
distinctparser junk = junkparser "usage: distinct  <column><condition><glob string> " junk

--datefiparser
datefixparser :: [Token] -> [Token] 
datefixparser tok@[(Ident str),(Date date)] = tok
datefixparser tok@[(Quotedstr str),(Date date)] = [(Ident str),(Date date)]
datefixparser junk = junkparser "usage: date-fix <column> <date>" junk

--gridfix
gridfixparser :: [Token] -> [Token] 
gridfixparser tok@[(Ident str),(Ident ne)] 	| isGridRef ne = [(Ident str),(Gridref ne)]
											| otherwise = junkparser "usage: <column> <grid ref>" tok

gridfixparser tok@[(Quotedstr str),(Ident ne)] 
											| isGridRef ne = [(Ident str),(Gridref ne)]
											| otherwise = junkparser "usage: <column> <grid ref>" tok

gridfixparser junk = junkparser "usage: grid-fix <column> <grid ref>" junk

isGridRef  (c:cs) | isAlpha c = isGridRef' cs
			      | otherwise = False

isGridRef _ = False
isGridRef' (d1:d2:d3:d4:d5:d6:ds) | isDigit d1 && isDigit d2 && isDigit d3 && isDigit d4 && isDigit d5 && isDigit d6 = True
								  | otherwise = False
isGridRef' _ = False

--reformat 
reformatparser toks@[(Ident str),Uppercase] = toks
reformatparser toks@[(Ident str),Capitalize] = toks
reformatparser toks@[(Ident str),Lowercase] = toks
reformatparser toks@[(Ident str),Trim] = toks
reformatparser toks@[(Quotedstr str),Uppercase] = toks
reformatparser toks@[(Quotedstr str),Capitalize] = toks
reformatparser toks@[(Quotedstr str),Lowercase] = toks
reformatparser toks@[(Quotedstr str),Trim] = toks

reformatparser junk = junkparser "usage: reformat <uppercase | capitalize | lowercase | trim>" junk

--sort
sortparser :: [Token] -> [Token]
sortparser [] = []
sortparser ((Ident str):Ascending:ts) =  sortparser ts ++ [(Ident str),Ascending]
sortparser ((Ident str):Descending:ts) = sortparser ts ++ [(Ident str),Ascending]
sortparser ((Quotedstr str):Ascending:ts) = sortparser ts ++ [(Ident str),Ascending]
sortparser ((Quotedstr str):Descending:ts) = sortparser ts ++ [(Ident str),Ascending]
sortparser junk = junkparser "usage: sort <column> <ascending | descending>" junk

--select
selectparser :: [Token] -> [Token]
selectparser [Ident all] | all == "all" = [Ident all]
selectparser toks = condparser "select ...<condition>...\n condition: <column><condition operator><quotedstring>" [] toks

--update
updateparser :: [Token] -> [Token]
updateparser toks@((Const num):(Quotedstr str):val:[])  = toks
updateparser toks@((Const num):(Ident str):val:[])  = toks
updateparser junk = junkparser "usage: update <row number> <column> <new value>" junk


--delete 
deleteparser tok@(Const val:[]) = tok
deleteparser junk = junkparser "usage: delete <row number>" junk

--insert
insertparser [] = []
insertparser ((Quotedstr name):(Condition cond):(Quotedstr str):ts) | cond == "=" = (ConditionStr name cond str): insertparser ts
insertparser junk = junkparser "usage insert ...<column=\"value\">..." junk

--junk
junkparser:: String -> [Token] -> [Token]
junkparser msg junk  = [Invalid msg]
