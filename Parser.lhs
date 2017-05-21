> data Parser a = Parser (String -> [(a, String)])

> instance Functor Parser where
>   fmap f (Parser p) = Parser (\str ->
>     [(f a, x) | (a, x) <- p str])  

> instance Applicative Parser where
>   pure a                    = Parser (\str -> [(a, str)])
>   (Parser f) <*> (Parser p) = Parser (\str -> [(g a, x') | (g, x) <- f str, (a, x') <- p x])

> instance Monad Parser where
>   (Parser p) >>= f = Parser (\str -> [p' | (a, x) <- p str, p' <- parse (f a) x])


> parse :: Parser a -> (String -> [(a, String)])
> parse (Parser p) = p

> char :: Parser Char 
> char = Parser (\str -> case str of
>   []     -> []
>   (x:xs) -> [(x, xs)])

> parseInt :: Parser Int
> parseInt = undefined
