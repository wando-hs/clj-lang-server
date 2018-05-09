module Main where

import System.IO
import Control.Monad
import Data.Void
import Data.Text
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr

type Parser = Parsec Void String
data Sexp = Stuff String | Sexp [Sexp]
  deriving (Show, Eq)

symbol :: Parser String
symbol = space *> some (try (noneOf "\n() "))

sexp :: Parser Sexp
sexp = space *> (fmap Sexp $ betweenChar $ some (try text <|> sexp))
  where text = fmap Stuff symbol
        betweenChar = between (char '(') (char ')')

--main :: IO ()
main = do
    handle <- openFile "resources/core.clj" ReadMode
    contents <- hGetContents handle
    parsed <- return $ parse (some $ try sexp) "" contents
    return $ parsed

