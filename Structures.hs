import Parser
import Combinators

import Control.Applicative

type Term = Integer 

term :: Parser Term
term = nat

data Expr = N Term
          | Add Expr Expr

expr :: Parser Expr
expr =  N   <$> nat 
    <|> Add <$> expr <*> expr
