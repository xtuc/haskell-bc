module Main where

import Debug.Trace

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language

--LiftIO
import Control.Monad.Trans

-- Console utilities
import System.Console.Haskeline

data Expr = Plus Expr Expr
          | Sub Expr Expr
          | Devide Expr Expr
          | Mult Expr Expr
          | Zero
          | String String
          | Numeric Integer
          | EvalError Expr
          deriving (Read, Show, Eq)

toString :: Expr -> String
toString (Plus l r) = "(+ " ++ toString l ++ ", " ++ toString r ++ ")"
toString (Sub l r) = "(- " ++ toString l ++ ", " ++ toString r ++ ")"
toString (Devide l r) = "(/ " ++ toString l ++ ", " ++ toString r ++ ")"
toString (Mult l r) = "(* " ++ toString l ++ ", " ++ toString r ++ ")"
--toString (Num x) = show x
toString (Numeric x) = show x
toString (EvalError x) = "Illegal expression " ++ toString x
toString x = show x

integerToExpr :: Integer -> Expr
integerToExpr x = Numeric x

-- TODO: return a monad instead of an integer
-- If current evaluation is not possible, 0 is returned
eval :: Expr -> Integer
eval ex = case ex of
  Mult l r -> eval l * eval r
  Plus l r -> eval l + eval r
  Sub l r -> eval l - eval r
  --Devide l r -> eval l / eval r
  Numeric n   -> n
  Zero -> 0
  _ -> 0

symbol x = Token.symbol lexer x
number = Token.natural lexer
ident = Token.identifier lexer

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser emptyDef

parseInfix :: Parser Expr
parseInfix = do
              exp <- psExp
              return exp

psExp :: Parser Expr
psExp = do
            left <- psTerm
            exp <- psExpStar left
            return exp

toExpr :: Char -> Expr -> Expr -> Expr
toExpr '+' l r = Plus l r
toExpr '-' l r = Sub l r
toExpr '/' l r = Devide l r
toExpr '*' l r = Mult l r
toExpr _ l r = Zero

--Parse calcul
parseOperation :: Char -> Expr -> Parser Expr
parseOperation op l = do
                            symbol [op]
                            r <- psFactor
                            append <- psTermStar $ toExpr op l r
                            return append

psTermStar :: Expr -> Parser Expr
psTermStar left = parseOperation '*' left
                    <|> parseOperation '/' left
                    <|> psExpStar left
                    <|> return left

psExpStar :: Expr -> Parser Expr
psExpStar left = parseOperation '+' left
                    <|> parseOperation '-' left
                    <|> return left

psTerm :: Parser Expr
psTerm = do
            left <- psFactor
            exp <- psTermStar left
            return exp

-- Parse all the stuff
psFactor :: Parser Expr
psFactor =
            (do {
                obj <- number;
                return (Numeric obj)
            })
            <|>
            (do {
                obj <- ident;
                return (String obj)
            })
            <|>
            (do {
                symbol "(";
                obj <- psExp;
                symbol ")";
                return obj
            })

parseExpr :: String -> Either ParseError Expr
parseExpr x = parse parseInfix "<stdin>" x

process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> case eval ex of
      --Nothing -> putStrLn "Cannot evaluate"
      --Just result -> putStrLn $ toString result
      result -> putStrLn $ toString $ integerToExpr result

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    result <- getInputLine "> "
    case result of
      Nothing -> outputStrLn "Goodbye."
      Just result -> (liftIO $ process result) >> loop