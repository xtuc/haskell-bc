module Main where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language

--LiftIO
import Control.Monad.Trans

--Forever
import Control.Monad

-- Console utilities
import System.Console.Haskeline

generateS_exp :: String -> String -> String -> String
generateS_exp op left right = "(" ++ op ++ " " ++ left ++ " " ++ right ++ ")"

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser emptyDef

symbol = Token.symbol lexer
number = Token.natural lexer
ident = Token.identifier lexer

parseInfix = (do {exp <- psExp; return exp})

psExp = (do {left <- psTerm; exp <- (psExpStar left); return exp})
psExpStar left = (do {symbol "+";
                      right <- psTerm;
                      append <- psExpStar (generateS_exp "+" left right);
                      return append})
             <|> (do {symbol "-";
                      right <- psTerm;
                      append <- psExpStar (generateS_exp "+" left right);
                      return append})
             <|> return left

psTerm = (do {left <- psFactor; exp <- (psTermStar left); return exp})
psTermStar left = (do {symbol "*";
                        right <- psFactor;
                        append <- psTermStar (generateS_exp "*" left right);
                        return append})
              <|> (do {symbol "/";
                        right <- psFactor;
                        append <- psTermStar (generateS_exp "/" left right);
                        return append})
              <|> return left

psFactor = (do {obj <- number; return (show obj)})
      <|> (do {obj <- ident; return obj})
      <|> (do {symbol "(";
                obj <- psExp;
                symbol ")";
                return obj})

convertI2S :: String -> Either ParseError String
convertI2S s = parse parseInfix "i2s" s

process :: String -> IO ()
process line = do
  let res = convertI2S line
  case res of
    Left err -> print err
    Right ex -> case eval ex of
      Nothing -> putStrLn "Cannot evaluate"
      Just result -> putStrLn $ ppexpr result

main :: IO ()
main = process ( getInputLine "> " )