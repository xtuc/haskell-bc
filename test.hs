module Main where

-- Mathematical operations,
-- in Haskell they don't have a given type. They are expremied as standart fonctions
data PLUS = (+)
data SUB = (-)
data DIVIDE = (/)
data MULT = (*)
--data OPEN_BRACKET = Char
--data CLOSE_BRACKET = Char

data Operation = PLUS | SUB | DIVIDE | MULT | Null deriving (Show, Eq)

--data Token = Operation | Element | Null deriving Eq
data ComputedResult = Maybe String deriving (Read)

data Token = Token { x :: Integer, o :: Operation } deriving (Show)
data AST = List[Token] | AST

-- Parse DSL into tokens
tokenize :: Char -> Token
tokenize c
    | c == '+' = Token 0 PLUS
    | c == '-' = Token 0 SUB
    | c == '*' = Token 0 MULT
    | c == '/' = Token 0 DIVIDE
    | otherwise = Token (toNum c) Null

--toAst :: [Token] -> AST
--toAst x = foldl (\acc e -> e : acc) [] x

toNum :: Char -> Integer
toNum x = read [x] :: Integer

interpreter :: IO String -> IO String
interpreter s = fmap (compute . parse) s

parse :: String -> [Token]
parse x = foldl (\acc e -> (map tokenize e) ++ acc) [] [x]

-- Used to filter input, we don't want chars in computing process
--isIntegral :: _ -> Bool
--isIntegral (Num _) = True
--isIntegral _ = False

tokenToString :: Token -> String
tokenToString (Token x Null) = show x :: String
tokenToString (Token _ x) = show x :: String

compute :: [Token] -> String
compute x = foldl (\acc e -> tokenToString e ++ "\n" ++ acc) "" x

--doCompute :: 

main :: IO ()
main = do
    result <- interpreter getLine
    putStrLn result
    main