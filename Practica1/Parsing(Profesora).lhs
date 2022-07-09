import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

-- The monad of parsers
--------------------

newtype Parser a              =  P (String -> [(a,String)])

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure = return
    (<*>) = ap
 
instance Monad Parser where
    return v                   =  P (\inp -> [(v,inp)])
    p >>= f                    =  P (\inp -> [ x | (v,out) <- parse p inp, x <- parse (f v) out])

instance Alternative Parser where
    empty = mzero
    (<|>) = mplus
 
instance MonadPlus Parser where
    mzero                      =  P (\_	 -> [])
    p `mplus` q                =  P (\inp -> case parse p inp of
                                                []        -> parse q inp
                                                x         -> x)

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp =  p inp

--------------------------------------------------------------------------------------------------------
------------------------------------- Desde acá comenzó la clase ---------------------------------------

-- Basic parsers
-------------

failure :: Parser a
failure = P (\_ -> [])


item :: Parser Char
item = P(\xs -> case xs of
	  	[] -> []
                (x:xs) -> [(x,xs)])

-- Derived primitives
------------------

sat :: (Char -> Bool) -> Parser Char
sat p = do c <- item
           if p c then return c
		  else failure							

digit :: Parser Char
digit = sat isDigit

alphanum :: Parser Char
alphanum =  sat isAlphaNum

char :: Char -> Parser Char
char x =  sat (== x)
 
string :: String -> Parser String
string [] = return []
string (x:xs) = do char x 
                   string xs 
                   return (x:xs) 


-- Parsers combinators
--------------------


many :: Parser a -> Parser [a]
many p = many1 p <|> return []   

many1 :: Parser a -> Parser [a]
many1 p = do x <- p
             xs <- many p
             return (x:xs)  
 
-- [1,2,3,4]
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do x <- p
                  xs <- many (do {sep;p} )
                  return (x:xs)


nat :: Parser Int
nat =  do xs <- many1 digit
          return (read xs)

-------------------------------------------------------------------------------
-- GLC

-- expr → term ’+’ expr | term
-- term → factor ’*’ term | factor
-- factor → digit | ’(’ expr ’)’
-- digit → ’0’ | ’1’ | · · · | ’9’

-- Factorizamos

digito :: Parser Int
digito = do d <- digit
            return (digitToInt d)

factor :: Parser Int
factor = do char '('
            e <- expr
            char ')'
            return e
        <|> digito     

-- term → factor (’*’ term | ε)
term :: Parser Int
term = do f <- factor 
          (do char '*'
              t <- term
              return (f*t)) <|> return f

-- expr → term (’+’ expr | ε)
{-
expr :: Parser Int
expr = do t <- term 
          (do char '+'
              e <- expr
              return (t+e)) <|> return t
 -}


-- Extendemos la gramática
-- expr → term (’+’ expr | '-' expr | ε)
-- term → factor (’*’ term | '/' term | ε)

-- Modificamos la gramática para que asocie a izquierda

-- expr → expr (’+’ term | '-' term) | term
-- term → term (’*’ factor | '/' factor) | term

-- Resolvemos el problema de la recursión a izquierda modificando la gramática
-- Dada la gramática:   A → A α | β          (donde α no es vacío y β no empieza con A)
--
-- La transformamos a : A → β A'
--                      A' → ε | α A'

-- expr → expr (’+’ term | '-' term) | term

-- expr -> term expr'
-- expr' -> ε | (’+’ term | '-' term) expr' 



-- ( (1 + 3) -2)

expr :: Parser Int
expr = do t <- term  
          f <- expr' 
          return (f t) 

expr' :: Parser (Int -> Int)
expr' = do char '+'         
           t <- term        
           f <- expr'      
           return (f . (+t))
        <|> do char '-'
               t <- term
               f <- expr'
               return (f . (\x -> x-t))
            <|> return id    
        

















