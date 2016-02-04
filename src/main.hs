module Main where

import Text.Parsec

import Control.Monad
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Text.PrettyPrint (Doc, (<>), (<+>))
import qualified Text.PrettyPrint as PP

import Data.Maybe
import Data.Functor

import Control.Monad.Trans
import System.Console.Haskeline

import Data.Functor.Identity

data Expr = Tr
          | Plus Expr Expr
          | Sub Expr Expr
          | Devide Expr Expr
          | Mult Expr Expr
          | Zero
          | IsZero Expr
          | Succ Expr
          | Double
          | Integer
          deriving (Eq, Show)

-- +---+---+---+---+---+---+---+--+
-- |   |   |   | x |   |   |   |  |
-- |   | + |   |   |   | + |   |  |
-- | 1 |   | 2 |   | 1 |   | 3 |  |
-- +---+---+---+---+---+---+---+--+

    --Token.plus = "+",
    --Token.sub = "-",
    --Token.devide = "/",
    --Token.mult = "*"

--DSL definition
langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef {
    Tok.commentStart    = "",
    Tok.commentEnd      = "",
    Tok.commentLine     = "",
    Tok.nestedComments  = False,
    Tok.identStart      = digit,
    Tok.identLetter     = digit,
    Tok.opStart         = oneOf "+-*/=",
    Tok.opLetter        = oneOf "+-*/=",
    Tok.reservedNames   = [],
    Tok.reservedOpNames = [],
    Tok.caseSensitive   = False
}

class Pretty p where
  ppr :: Int -> p -> Doc

instance Pretty Expr where
  ppr _ Zero = PP.text "0"
  ppr p (Plus a b) =
        PP.text "+"   <+> ppr p a
    <+> PP.text "+" <+> ppr p b
  --ppr p (Plus a b) = PP.text a ++ "+" ++ PP.text b
  --ppr _ Sub = PP.text "-"
  --ppr _ Devide = PP.text "/"
  --ppr _ Mult = PP.text "*"

ppexpr :: Expr -> String
ppexpr = PP.render . ppr 0

isNum :: Expr -> Bool
isNum Zero     = True
isNum (Succ t) = isNum t
isNum _        = False

isVal :: Expr -> Bool
isVal Tr = True
isVal t | isNum t = True
isVal _ = False

eval' :: Expr -> Maybe Expr
eval' x = case x of
  IsZero Zero               -> Just Tr
  IsZero t                  -> IsZero <$> (eval' t)
  Succ t                    -> Succ <$> (eval' t)
  --Plus t                    -> Succ <$> (eval' t)
  _                         -> Nothing

nf :: Expr -> Expr
nf x = fromMaybe x (nf <$> eval' x)

eval :: Expr -> Maybe Expr
eval t = case nf t of
  nft | isVal nft -> Just nft
      | otherwise -> Nothing -- term is "stuck"


lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

addop :: Parser (Expr -> Expr -> Expr)
addop = (infixOp "+" Plus)

-- Prefix operators
table :: Ex.OperatorTable String () Identity Expr
table = [
    [
      prefixOp "+" Succ
    , prefixOp "iszero" IsZero
    ]
  ]


-- Constants
zero :: Parser Expr
--zero, plus, sub, devide, mult :: Parser Expr
zero  = reservedOp "0"   >> return Zero
--sub = reservedOp "-" >> return Sub
--devide = reservedOp "/" >> return Devide
--mult = reservedOp "*" >> return Mult

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

factor :: Parser Expr
factor =
        zero
        <|> plusExpr
        <|> parens expr

plusExpr :: Parser Expr
plusExpr = do
    reserved "+"
    v1 <- expr
    reservedOp "+"
    v2 <- expr
    return (Plus v1 v2)

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> case eval ex of
      Nothing -> putStrLn "Cannot evaluate"
      Just result -> putStrLn $ ppexpr result

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    result <- getInputLine "> "
    case result of
      Nothing -> outputStrLn "Goodbye."
      Just result -> (liftIO $ process result) >> loop