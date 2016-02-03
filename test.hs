module Main where

import Control.Monad

-- +---+---+---+---+---+---+---+--+
-- |   |   |   | x |   |   |   |  |
-- |   | + |   |   |   | + |   |  |
-- | 1 |   | 2 |   | 1 |   | 3 |  |
-- +---+---+---+---+---+---+---+--+

-- Mathematical operations,
-- in Haskell they don't have a given type. They are expremied as standart fonctions
data PLUS = (+)
data SUB = (-)
data DIVIDE = (/)
data MULT = (*)
--data OPEN_BRACKET = Char
--data CLOSE_BRACKET = Char

data Operation = PLUS | SUB | DIVIDE | MULT | Null deriving (Show, Eq)

data ComputedResult = Maybe String deriving (Read)

data Token = Token { x :: Integer, o :: Operation } deriving (Show, Eq)

-- Parse DSL into tokens
tokenize :: Char -> Token
tokenize c
    | c == '+' = Token 0 PLUS
    | c == '-' = Token 0 SUB
    | c == '*' = Token 0 MULT
    | c == '/' = Token 0 DIVIDE
    | otherwise = Token (toNum c) Null

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
compute tokens = foldl (\acc e -> tokenToString e ++ "\n" ++ acc) "" tokens
--compute tokens = do
--    let ast = []
--    _ <- fmap (\el -> do
--                if tokenToString el == "MULT" then
--                    el : ast
--                else
--                    ast) tokens
--    ""
    

main :: IO ()
main = do
    putStr "BC\n"
    forever $ do
    putStr "> "
    result <- interpreter getLine
    putStrLn result
