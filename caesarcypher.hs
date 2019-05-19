import Data.Char

encode :: String -> String
encode [] = []
encode (x:xs) = (cypher [x]) ++ encode (xs)

cypher :: String -> String
cypher x
  | x == "a" = "x"
  | x == "b" = "y"
  | x == "c" = "z"
  | x == "d" = "a"
  | x == "e" = "b"
  | x == "f" = "c"
  | x == "g" = "d"
  | x == "h" = "e"
  | x == "i" = "f"
  | x == "j" = "g"
  | x == "k" = "h"
  | x == "l" = "i"
  | x == "m" = "j"
  | x == "n" = "k"
  | x == "o" = "l"
  | x == "p" = "m"
  | x == "q" = "n"
  | x == "r" = "o"
  | x == "s" = "p"
  | x == "t" = "q"
  | x == "u" = "r"
  | x == "v" = "s"
  | x == "w" = "t"
  | x == "x" = "u"
  | x == "y" = "v"
  | x == "z" = "w"
  | otherwise = x
  
decode :: String -> String
decode [] = []
decode (x:xs) = (decypher [x]) ++ decode (xs)

decypher :: String -> String
decypher x
  | x == "x" = "a"
  | x == "y" = "b"
  | x == "z" = "c"
  | x == "a" = "d"
  | x == "b" = "e"
  | x == "c" = "f"
  | x == "d" = "g"
  | x == "e" = "h"
  | x == "f" = "i"
  | x == "g" = "j"
  | x == "h" = "k"
  | x == "i" = "l"
  | x == "j" = "m"
  | x == "k" = "n"
  | x == "l" = "o"
  | x == "m" = "p"
  | x == "n" = "q"
  | x == "o" = "r"
  | x == "p" = "s"
  | x == "q" = "t"
  | x == "r" = "u"
  | x == "s" = "v"
  | x == "t" = "w"
  | x == "u" = "x"
  | x == "v" = "y"
  | x == "w" = "z"
  | otherwise = x
