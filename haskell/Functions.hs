module Functions
	where


launch = countreplicas ["a","a","a","b","b"]

countreplicas ::[String] -> String
countreplicas (c:cs)= countreplicas' 1 c cs

countreplicas':: Int -> String -> [String] -> String
countreplicas' num last [] = last ++": "++ (show num) ++ "\n"
countreplicas' num last (c:cs) 	| c == last = countreplicas' (num+1) last cs
					   			| otherwise =(last ++": "++ (show num) ++ "\n") ++ (countreplicas' 1 c cs)
