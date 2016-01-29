-- Mathematical operations,
-- in Haskell they don't have a given type. They are expremied as classic fonctions
data PLUS = (+)
data SUB = (-)
data DIVIDE = (/)
data MULT = (*)
--data OPEN_BRACKET = Char
--data CLOSE_BRACKET = Char

data Operation = PLUS | SUB | DIVIDE | MULT
data Element = Char
data Token = Operation | Element | Null
data ComputedResult = Maybe String deriving Show

-- Parse DSL into tokens
tokenize :: Char -> Token
tokenize c
    | c == '*' = MULT
    | c == '+' = PLUS
    | otherwise = c

--toNum :: String -> Num
--toNum x = read x :: Num

interpreter :: IO String -> IO String
interpreter s = fmap (compute . parse) s

parse :: String -> [Token]
parse x = foldl (\acc e -> fmap (tokenize) e : acc) [] [x]

-- Used to filter input, we don't want chars in computing process
--isIntegral :: _ -> Bool
--isIntegral (Num _) = True
--isIntegral _ = False

compute :: [Token] -> String
compute a = "Computed: " ++ foldr (++) "" a

main :: IO ()
main = do
    result <- interpreter getLine
    putStrLn result
    main