module Main where

import System.IO
import Control.Monad
import Data.Void
import Data.Text
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr

data Sexp = Function String String
type Parser = Parsec Void String

symbol :: Parser String
symbol = space *> some (noneOf ")\n\t\r ")

sexp :: Parser [String]
sexp = between (char '(') (char ')') $ some symbol

--main :: IO ()
main = do
    handle <- openFile "resources/core.clj" ReadMode
    contents <- hGetContents handle
    parsed <- return $ parse sexp "" contents
    return $ parsed

