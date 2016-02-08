module Main where

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

class Pretty p where
  ppr :: Int -> p -> Doc

instance Pretty Expr where
  ppr _ Zero = PP.text "0"
  ppr _ String = PP.text "?"
  --ppr p (Plus a b) =
  --      PP.text "+" <+> ppr p a
  --  <+> PP.text "+" <+> ppr p b

toString :: Expr -> String
toString (Plus l r) = "(+ " ++ toString l ++ " " ++ toString r ++ ")"
toString x = show x

toExpr :: String -> Expr
toExpr x = read x :: Expr

intToExpr :: Integer -> Expr
intToExpr x = Num x

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser emptyDef

symbol = Token.symbol lexer
number = Token.natural lexer
ident = Token.identifier lexer

parseInfix = do
              exp <- psExp
              return exp

--psExp :: _ -> String
psExp = do
            left <- psTerm
            exp <- psExpStar left
            return exp

--Parse calcul
parseOperation :: String -> Expr -> Parser Expr
parseOperation op l = do
                            symbol op
                            r <- psFactor
                            append <- psTermStar $ Plus l r
                            return append

psExpStar :: Expr -> Parser Expr
psExpStar left = parseOperation "+" left
                    <|> parseOperation "-" left
                    <|> return left

psTermStar :: Expr -> Parser Expr
psTermStar left = parseOperation "*" left
                    <|> parseOperation "/" left
                    <|> return left

psTerm = do
            left <- psFactor
            exp <- psTermStar left
            return exp

--Number conversion
psFactor = 
            (do {
                obj <- number;
                return (intToExpr obj)
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

--eval :: String -> Maybe String
--eval t = return t

convertI2S :: String -> Either ParseError Expr
convertI2S x = parse parseInfix "i2s" x

--interpreter :: IO String -> IO String
--interpreter x = convertI2S x

--main :: IO ()
--main = interpreter $ getInputLine "> "