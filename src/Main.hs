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

functionHeader :: Parser String
functionHeader = someTill anyChar (char '[' )

main :: IO ()
main = do
    handle <- openFile "resources/core.clj" ReadMode
    contents <- hGetContents handle
    parsed <- parseTest functionHeader contents
    hClose handle
    return parsed
