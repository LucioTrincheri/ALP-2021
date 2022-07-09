import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

-- Ejercicio 1

-- sepBy intenta parsear una entrada mediante un parser "a" que debe cumplir cada elemento de la entrada y
-- un parser "b" separador que separa los elementos de la entrada. Si se encuentra un elemento que cumple
-- con el parser "b", se parsea ese elemento (sepBy devuelve una lista de elementos parseados). Si en algun
-- momento no se cumple el parser "a" en cada elemento parseado, entonces se devuelve el parseo vacio [].

-- Ejercicio 2

-- expr → term (’+’ expr | ’-’ expr | ε)
-- term → factor (’*’ term | ’/’ term | ε)
-- factor → digit | ’(’ expr ’)’
-- digit → ’0’ | ’1’ | · · · | ’9’
--------------
-- digit → ’0’ | ’1’ | · · · | ’9’
digito :: Parser Int
digito = do d <- digit
            return (digitToInt d)

-- factor → digit | ’(’ expr ’)’
factor :: Parser Int
factor = do char '('
            e <- expr
            char ')'
            return e
         <|> digito     

-- term → factor (’*’ term | ’/’ term | ε)
term :: Parser Int
term = do f <- factor 
          (do char '*'
              t <- term
              return (f*t)) <|> 
              (do char '/'
                  t <- term
                  return (div f t)) <|> return f

-- expr → term (’+’ expr | ’-’ expr | ε)
expr :: Parser Int
expr = do t <- term 
          (do char '+'
              e <- expr
              return (t+e)) <|> 
              (do char '-'
                  e <- expr
                  return (t-e)) <|> return t

-- Ejercicio 3
parT :: Parser a -> Parser a
parT p = do char '('
            t <- parT p
            char ')'
            return t
            <|> p
result3 = parse (parT nat) "((423952))"