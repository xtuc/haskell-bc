data Expression = Expression String | Math

expression :: Char -> Expression
expression c
    | c == '*' = "fois"
    | otherwise = [c]

--data ComputedResult = String
--data ComputedResult = string | Error

--compute (s:xs) = s ++ compute xs
--compute [] = ""

interpreter :: IO String -> IO String
interpreter s = fmap (compute . parse) s

-- Should be better with guards
--parse :: [Char] -> String
--parse ('*':xs) = "fois" ++ parse xs
--parse ('+':xs) = "plus" ++ parse xs
--parse (x:xs) = [x] ++ parse xs
--parse _ = ""

parse :: String -> [String]
parse x = foldl (\acc e -> expression e : acc) [] [x]


-- Used to filter input, we don't want chars in computing process
--isIntegral :: _ -> Bool
--isIntegral ( _) = True
--isIntegral _ = False

compute :: [String] -> String
compute a = "Computed: " ++ foldr (++) "" a

main :: IO ()
main = do
    result <- interpreter getLine
    putStrLn result
    main

--exp = exp ope exp | elm 
--ope = * / - +
--elm = 1-9