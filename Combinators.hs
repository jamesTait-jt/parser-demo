module Combinators where

import Parser

import Data.Char
import qualified Control.Applicative as Applicative ( Applicative(..), Alternative(..) ) 

-- item will parse a single character from a string.
item :: Parser Char
item = Parser (\str -> case str of
                         []     -> []         -- if the input string is empty, return the empty list
                         (x:xs) -> [(x, xs)]) -- if not, parse the head of the list
  
-- satisfy takes in a function, if parses some input, if the input satisfies the
-- function, the parser is returned, if not, the failre parser is returned.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do 
  c <- item
  if p c 
     then produce c
     else failure

char :: Char -> Parser Char
char c = satisfy (c==) 

-- First parses the char, at the head of the string, then recursively calls
-- itself to move through the string, once everything has been parsed, returns a
-- parser for that string
string :: String -> Parser String
string []     = produce []
string (c:cs) = do 
  char c
  string cs
  produce (c:cs)

-- if the parsed char is an in the string provided, parse it, if not, fail.
oneOf :: [Char] -> Parser Char
oneOf chars = satisfy (`elem` chars)

some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p

many :: Parser a -> Parser [a]
many p = some p Applicative.<|> produce []

alpha :: Parser Char
alpha = oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z']

plus :: Parser Char
plus = char '+'

nat :: Parser Integer
nat = read <$> some (satisfy isDigit)
