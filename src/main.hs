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
          | Double
          | String
          deriving (Read, Show)

toString :: Expr -> String
toString (Plus l r) = "(+ " ++ toString l ++ " " ++ toString r ++ ")"
toString x = show x

toExpr :: String -> Expr
toExpr x = read x :: Expr

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser emptyDef

symbol = Token.symbol lexer
number = Token.natural lexer
ident = Token.identifier lexer

parseInfix = do
              exp <- psExp
              return exp

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

psTerm = do
            left <- psFactor
            exp <- psTermStar left
            return exp

-- Parse all the stuffs
psFactor = 
            (do {
                obj <- number;
                return (Num obj)
            })
            <|>
            (do {
                obj <- ident;
                return (toExpr obj)
            })
            <|>
            (do {
                symbol "(";
                obj <- psExp;
                symbol ")";
                return obj
            })

convertI2S :: String -> Either ParseError Expr
convertI2S x = parse parseInfix "i2s" x

--interpreter :: IO String -> IO String
--interpreter x = convertI2S x

--main :: IO ()
--main = interpreter $ getInputLine "> "