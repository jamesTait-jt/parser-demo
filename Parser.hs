module Parser where

import qualified Control.Applicative as Applicative ( Applicative(..), Alternative(..) )

-- A parser reads in some output. It then parses something, and returns the
-- parsed result, aswell as the rest of the input. This data type acknowledges
-- that the parser will sometimes fail. 
data Parser a = Parser (String -> [(a, String)])

-- In order for out parser to be useful, we need it to be an instance of a
-- functor. This will allow us to fmap over the structure.
instance Functor Parser where
  fmap f (Parser p) = Parser (\str -> [(f a, x) | (a, x) <- p str])  

-- We also ned applicatives to be able to sequence actions while maintaining the
-- parser structure.
instance Applicative Parser where
  pure                      = produce
  (Parser f) <*> (Parser p) = Parser (\str -> [(g a, x') | (g, x) <- f str, (a, x') <- p x])

-- This allows us to choose between parsers. If one fails, the other one will be
-- applied.
instance Applicative.Alternative Parser where
  empty = failure
  p <|> q = orElse p q

orElse :: Parser a -> Parser a -> Parser a
orElse (Parser px) (Parser py) = Parser (\str -> case px str of
                                                   [] -> py str
                                                   xs -> xs)

instance Monad Parser where
  (Parser p) >>= f = Parser (\str -> [p' | (a, x) <- p str, p' <- parse (f a) x])

-- Produce is the parser that consumes no input.
produce :: a -> Parser a
produce x = Parser (\str -> [(x, str)])


--   An example of produce and parse working together is shown as follows:
--    
--   parse (produce 42) "hello" -> [(42, "hello")]


-- Failure is the parser that always fails.
failure :: Parser a
failure = Parser (\str -> [])

-- failure works like this
--
-- parse failure "hello" -> []

-- Applies the function wrapped inside the parser datatype.
parse :: Parser a -> (String -> [(a, String)])
parse (Parser p) = p
