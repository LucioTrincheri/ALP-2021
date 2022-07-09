module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "else", "repeat", "skip", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )


x <||> y = (try x) <|> y
----------------------------------
--- Parser de expressiones enteras
-----------------------------------
intexp :: Parser (Exp Int)
intexp = do chainl1 equalIntexp commasepIntexp
intexpOperators :: Parser (Exp Int)
intexpOperators = do chainl1 compTerm addsubIntexp <||> (do parens lis $ intexp)

compTerm :: Parser (Exp Int)
compTerm = do chainl1 (parens lis intexp <||> do { x <- natural lis ; return (Const (fromInteger x))} <||> do { x <- identifier lis ; return (Var x)} <|> unarynegationIntexp) timesdivIntexp

commasepIntexp :: Parser (Exp Int -> Exp Int -> Exp Int)
commasepIntexp = do
                   reservedOp lis ","
                   return (ESeq)

equalIntexp :: Parser (Exp Int)
equalIntexp = do
                nombre <- identifier lis
                reservedOp lis "="
                x <- equalIntexp
                return (EAssgn nombre x)
                <||> intexpOperators

addsubIntexp :: Parser (Exp Int -> Exp Int -> Exp Int)
addsubIntexp = do 
                 reservedOp lis "+"
                 return (Plus)
               <|> do 
                 reservedOp lis "-"
                 return (Minus)
timesdivIntexp :: Parser (Exp Int -> Exp Int -> Exp Int)
timesdivIntexp = do 
                   reservedOp lis "*"
                   return (Times)
                 <|> do 
                   reservedOp lis "/"
                   return (Div)

unarynegationIntexp = do 
                        reservedOp lis "-"
                        n <- intexp
                        return (UMinus n)
-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = chainl1 (parens lis boolexp <||> do notBoolexp) orandBoolexp

orandBoolexp :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
orandBoolexp =  do
                  reservedOp lis "||"
                  return (Or)
                <|> do
                  reservedOp lis "&&"
                  return (And)

notBoolexp :: Parser (Exp Bool)
notBoolexp = 
    do
      reservedOp lis "!"
      x <- parens lis boolexp <|> do caseBoolexp
      return (Not x)
    <||> do
      x <- caseBoolexp
      return (x)

caseBoolexp :: Parser (Exp Bool)
caseBoolexp =
    -- First case: comparators
    do 
      termino1 <- intexp
      (do
         reservedOp lis ">"
         termino2 <- intexp
         return (Gt termino1 termino2)
       <|> do
         reservedOp lis "<"
         termino2 <- intexp
         return (Lt termino1 termino2)
       <|> do
         reservedOp lis "!="
         termino2 <- intexp
         return (NEq termino1 termino2)
       <|> do
         (reservedOp lis "==")
         termino2 <- intexp
         return (Eq termino1 termino2))
    -- Second case: value (lower priority)
    <||> do 
      (do
         reservedOp lis "true"
         return (BTrue)
       <|> do
         reservedOp lis "false"
         return (BFalse))
      
-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = chainl1 comm' (do { reservedOp lis ";" ; return (Seq) })

comm' :: Parser Comm
comm' = 
    do 
      reservedOp lis "skip"
      return Skip
    <|> do
      nombre <- identifier lis
      reservedOp lis "="
      valor <- intexp
      return (Let nombre valor)
    <|> 
      ifthen_elseComm
    <|> do
      reservedOp lis "repeat"
      termino1 <- braces lis comm
      reservedOp lis "until"
      termino2 <- boolexp
      return (Repeat termino1 termino2)
    

ifthen_elseComm :: Parser Comm
ifthen_elseComm = do 
                    reservedOp lis "if"
                    termino1 <- boolexp
                    termino2 <- braces lis comm
                    (do 
                      reservedOp lis "else"
                      termino3 <- braces lis comm
                      return (IfThenElse termino1 termino2 termino3)
                     <||> do
                       return (IfThen termino1 termino2))
------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)