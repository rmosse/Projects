module Lexer

	where

import Data.Char
data Token
	= Load | Save | Report | Count | List | Distinct | Output | NoOutput | DateFix 	| GridFix 
		| Reformat | Sort | Select | Show | Update | Delete | Insert | Help | Quit | Junk String 
		| Quotedstr String | Ident String | Registrations | Completions | ColumnNum Integer | Const Float
		deriving (Eq,Ord,Show)

scan :: String -> [Token]
scan = scan' . skip

skip "" = ""
skip (c:cs)
 | c == ' '  =  skip cs
 | otherwise  =  c:cs

scan' "" = [] 
--cmds
scan' ('l':'o':'a':'d':cs) = Load: scan cs
scan' ('s':'a':'v':'e':cs) = Save: scan cs
scan' ('"':cs) = quotelexer [] cs
scan' ('r':'e':'p':'o':'r':'t':cs) = Report: scan cs
scan' ('c':'o':'u':'n':'t':cs) = Count: scan cs
scan' ('l':'i':'s':'t':cs) = List: scan cs
scan' ('d':'i':'s':'t':'i':'n':'c':'t':cs) = Distinct: scan cs
scan' ('o':'u':'t':'p':'u':'t':cs) = Output: scan cs
scan' ('n':'o':'o':'u':'t':'p':'u':'t':cs) = NoOutput: scan cs
scan' ('d':'a':'t':'e':'f':'i':'x':cs) = DateFix: scan cs
scan' ('g':'r':'i':'d':'f':'i':'x':cs) = GridFix: scan cs
scan' ('r':'e':'f':'o':'r':'m':'a':'t':cs) = Reformat: scan cs
scan' ('s':'o':'r':'t':cs) = Sort: scan cs
scan' ('s':'e':'l':'e':'c':'t':cs) = Select: scan cs
scan' ('s':'h':'o':'w':cs) = Show: scan cs
scan' ('u':'p':'d':'a':'t':'e':cs) = Update: scan cs
scan' ('d':'e':'l':'e':'t':'e':cs) = Delete: scan cs
scan' ('i':'n':'s':'e':'r':'t':cs) = Insert: scan cs
scan' ('h':'e':'l':'p':cs) = Help: scan cs
scan' ('q':'u':'i':'t':cs) = Quit: scan cs
--args
scan' ('r':'e':'g':'i':'s':'t':'r':'a':'t':'i':'o':'n':'s':cs) = Registrations: scan cs
scan' ('c':'o':'m':'p':'l':'e':'t':'i':'o':'n':'s':cs) = Completions: scan cs
scan' ('$':cs) = columnnumlexer [] cs


scan' (c:cs)
 | isAlpha c  =  alphalexer [c] cs
 | isDigit c  =  floatlexer [c] cs

scan' jnk = junklexer [] jnk



columnnumlexer sofar "" = [ColumnNum (read sofar)]
columnnumlexer sofar (c:cs) 
	| isDigit c = columnnumlexer (sofar++[c]) cs
	| otherwise = (Junk (sofar++[c])): scan cs
							





junklexer sofar "" = [Junk (reverse sofar)]
junklexer sofar str@(c:cs)
	| c == ' '  =  (Junk (reverse sofar)):scan str
	| otherwise  =  junklexer (c:sofar) cs

qoutelexer sofar "" = [Quotedstr (sofar)]
quotelexer sofar ('"':cs) = (Quotedstr (sofar)):scan cs 
quotelexer sofar (c:cs) = quotelexer (sofar++[c]) cs

alphalexer sofar "" = [Ident (reverse sofar)]
alphalexer sofar str@(c:cs)
 | isAlpha c  =  alphalexer (c:sofar) cs
 | isDigit c  =  alphalexer (c:sofar) cs
 | c == '_'   =  alphalexer (c:sofar) cs
 | otherwise  =  (Ident (reverse sofar)) : scan str
 

floatlexer sofar "" = [Const (read (reverse sofar))]
floatlexer sofar str@(c:cs)
 | isDigit c  =  floatlexer (c:sofar) cs
 | c == '.'   =  dotlexer (c:sofar) cs
 | otherwise  =  (Const (read (reverse sofar))) : scan str


dotlexer sofar "" = [Junk (reverse sofar)]
dotlexer sofar str@(c:cs)
 | isDigit c  =  dotlexer' (c:sofar) cs

dotlexer' sofar "" = [Const (read (reverse sofar))]
dotlexer' sofar str@(c:cs)
 | isDigit c  =  dotlexer' (c:sofar) cs
 | otherwise  =  (Const (read (reverse sofar))) : scan str




