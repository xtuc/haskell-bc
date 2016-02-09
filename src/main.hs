module Main where

import Debug.Trace

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language

import Text.PrettyPrint (Doc, (<>), (<+>))
import qualified Text.PrettyPrint as PP

--LiftIO
import Control.Monad.Trans

--Forever
import Control.Monad

-- Console utilities
import System.Console.Haskeline

data Expr = Plus Expr Expr
          | Sub Expr Expr
          | Devide Expr Expr
          | Mult Expr Expr
          | Zero
          | Num Integer
          | String String
          deriving (Read, Show, Eq)

toString :: Expr -> String
toString (Plus l r) = "(+ " ++ toString l ++ " " ++ toString r ++ ")"
toString (Sub l r) = "(- " ++ toString l ++ " " ++ toString r ++ ")"
toString (Devide l r) = "(/ " ++ toString l ++ " " ++ toString r ++ ")"
toString (Mult l r) = "(* " ++ toString l ++ " " ++ toString r ++ ")"
toString x = show x

stringToExpr :: String -> Expr
stringToExpr x = read x :: Expr

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

createExpr :: Char -> Expr -> Expr -> Expr
createExpr '+' l r = Plus l r
createExpr '-' l r = Sub l r
createExpr '/' l r = Devide l r
createExpr '*' l r = Mult l r
createExpr _ l r = Zero

--Parse calcul
parseOperation :: Char -> Expr -> Parser Expr
parseOperation op l = do
                            symbol [op]
                            r <- psFactor
                            append <- psTermStar $ createExpr op l r
                            return append

psExpStar :: Expr -> Parser Expr
psExpStar left = parseOperation '+' left
                    <|> parseOperation '-' left
                    <|> return left

psTermStar :: Expr -> Parser Expr
psTermStar left = parseOperation '*' left
                    <|> parseOperation '/' left
                    <|> return left

psTerm :: Parser Expr
psTerm = do
            left <- psFactor
            exp <- psTermStar left
            return exp

-- Parse all the stuffs
psFactor :: Parser Expr
psFactor =
            (do {
                obj <- number;
                return (Num obj)
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
    Right ex -> case ex of
      --Nothing -> putStrLn "Cannot evaluate"
      --Just result -> putStrLn result
      result -> putStrLn $ toString result

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    result <- getInputLine "> "
    case result of
      Nothing -> outputStrLn "Goodbye."
      Just result -> (liftIO $ process result) >> loop