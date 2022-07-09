{ 
module Parse where

import Common
import Data.Char
import Data.List
}

%name func 
%tokentype { Token } 
%monad { E } { thenE } { returnE }


-- Tokens del parser
%token
    PR          { TPr $$ }
    BT          { TBottom }
    TOP         { TTop }
    '!'         { TNot }
    '&'         { TAnd }
    '|'         { TOr }
    '->'        { TThen }
    E           { TExists }
    A           { TAll }
    U           { TUntil } 
    AX          { TANext }
    EX          { TENext }
    AF          { TAFinally }
    EF          { TEFinally }
    AG          { TAGlobal }
    EG          { TEGlobal }
    '('         { TParenLeft}
    ')'         { TParenRight }
    '['         { TBracketL }
    ']'         { TBracketR }
    ','         { TComma }
    '{'         { TLeftBrace }
    '}'         { TRightBrace }
    ':'         { TDDot }
    ';'         { TSemicolon }
    '='         { TEqual }
    string      { TString $$ }
    States      { TStates }
    Transitions { TTransitions }
    Valuations  { TValuations }
    Exp         { TExp }
    Exit        { TExit }
    ErrorC      { TErrC }
    ErrorM      { TErrM }
    ErrorE      { TErrE }

%left '&' '|' 
%left '->' 
%nonassoc '!' AF EF AG EG AX EX AE E A

%%

-- Reglas de produccion de la gramatica

fields :: { Comm }
fields :  States      sts     { States $2 }  
       |  Transitions trns    { Transitions $2 } 
       |  Valuations  vals    { Valuations $2 } 
       |  Exp         ctl     { CTL $2 }
       |  Exit                { Exit }

-- States -----------------------------------------------------
sts : '[' states ']'    { $2 }
    | '[' ']'           { [] } 

states :: { [State] } 
states : string                   { [$1] }
       | states ',' string        { $3 : $1 }

-- Transitions ------------------------------------------------
trns : '[' transitions ']'  { $2 }
     | '[' ']'              { [] }

transitions :: { [Transition] }
transitions : transition                    { [$1] }
            | transitions ',' transition    { $3 : $1 }

transition : '(' string ',' string ')'  { ($2, $4) } 

-- Valuations -------------------------------------------------
vals :  '[' valuations ']'       { $2 }
     |  '[' ']'                  { [] }

valuations :: { [Valuation] }
valuations : valuation                     { [$1] }
           | valuations ',' valuation      { $3 : $1 }

valuation : '(' string ',' string ')'  { ($2, $4) } 

-- Operadores CTL según sintaxis provista por catedra y wikipedia CTL.
ctl :: { CTL }
ctl : PR                        { Prop $1 }
    | BT                        { Bottom }
    | TOP                       { Top }
    | '!' ctl                   { Not $2 }
    | ctl '&' ctl               { And $1 $3 }
    | ctl '|' ctl               { Or $1 $3 }
    | ctl '->' ctl              { Then $1 $3 }
    | AX ctl                    { AX $2 }
    | EX ctl                    { EX $2 }
    | A '[' ctl U ctl ']'       { AU $3 $5 }
    | E '[' ctl U ctl ']'       { EU $3 $5 }
    | AF ctl                    { AF $2 }
    | EF ctl                    { EF $2 }
    | AG ctl                    { AG $2 }
    | EG ctl                    { EG $2 }
    | '(' ctl ')'               { $2 }


-- Tipo de dato Tokens que matchea los del parser. La entrada se tokeniza con los mismos.
{
data Token =  TPr String
            | TBottom
            | TTop
            | TNot
            | TAnd
            | TOr
            | TThen
            | TANext
            | TENext
            | TAll
            | TExists
            | TUntil
            | TAFinally
            | TEFinally
            | TAGlobal
            | TEGlobal
            | TParenLeft
            | TParenRight 
            | TBracketL
            | TBracketR
            | TComma
            | TState String
            | TRightBrace
            | TLeftBrace
            | TDDot
            | TSemicolon
            | TEqual
            | TStates
            | TTransitions
            | TValuations
            | TString String
            | TExp
            | TExit
            | TErrC
            | TErrM
            | TErrE

            deriving (Show, Eq)

-- Funcion llamada de main encargada de lexear y parsear una linea
parseModel :: String -> Comm
parseModel contents = case func $ lexerComm contents of
                        Ok ctl -> ctl
                        Failed error -> ParseError error

-- Funcion requerida por el monada encargado de los errores de parseo. Se encarga de imprimir los mismos
happyError tokens | (head tokens == TErrC) = failE "Caracter invalido en comando. Ej comando: \"STATES\", \"TRANSITIONS\", \"VALUATIONS\", \"CTLEXP\", \"Exit\".\n"
                  | (head tokens == TErrM) = failE "Caracter invalido en modelo. Expresiones permitidas en el manual de uso.\n"
                  | (head tokens == TErrE) = failE "Caracter invalido en formula. Expresiones permitidas en el manual de uso.\n"
                  | otherwise = failE "Caracteres validos pero expresiones mal formadas. Referirse al manual de uso para ver el formato de las expresiones.\n"

-- Main lexer
lexerComm :: String -> [Token]
lexerComm [] = []
lexerComm cs@(c:cc) | isSpace c = lexerComm cc
                    | otherwise = case span isAlphaNum cs of
                                       ("Exit", rest) -> [TExit]
                                       ("STATES", rest) -> TStates : lexerModel rest
                                       ("TRANSITIONS", rest) -> TTransitions : lexerModel rest
                                       ("VALUATIONS", rest) -> TValuations : lexerModel rest
                                       ("CTLEXP", rest) -> TExp : lexerExpr rest
                                       otherwise -> [TErrC]

lexerModel [] = []
lexerModel cs@(c:cc) | isSpace c = lexerModel cc
                     | c == '['  = TBracketL : lexerModel cc
                     | c == ']'  = TBracketR : lexerModel cc
                     | c == ','  = TComma : lexerModel cc
                     | c == '('  = TParenLeft : lexerModel cc
                     | c == ')'  = TParenRight : lexerModel cc
                     | c == ')'  = TParenRight : lexerModel cc
                     | otherwise = case span isAlphaNum cs of
                                        (v, rest) -> if v /= "" then TString v : lexerModel rest else [TErrM] 
                                        otherwise -> [TErrM] 

lexerExpr [] = []
lexerExpr cs@(c:cc)  | isSpace c = lexerExpr cc
                     | c == '!'  = TNot : lexerExpr cc
                     | c == '&'  = TAnd : lexerExpr cc
                     | c == '|'  = TOr : lexerExpr cc
                     | c == '['  = TBracketL : lexerExpr cc
                     | c == ']'  = TBracketR : lexerExpr cc
                     | c == '('  = TParenLeft : lexerExpr cc
                     | c == ')'  = TParenRight : lexerExpr cc
                     | c == 'U'  = TUntil : lexerExpr cc
                     | otherwise = case cs of
                                        ('B':('T':ss)) -> TBottom : lexerExpr ss
                                        ('T':('O':('P':ss))) -> TTop : lexerExpr ss
                                        ('-':('>':ss)) -> TThen : lexerExpr ss
                                        ('A':('X':ss)) -> TANext : lexerExpr ss
                                        ('E':('X':ss)) -> TENext : lexerExpr ss
                                        ('A':('F':ss)) -> TAFinally : lexerExpr ss
                                        ('E':('F':ss)) -> TEFinally : lexerExpr ss
                                        ('A':('G':ss)) -> TAGlobal : lexerExpr ss
                                        ('E':('G':ss)) -> TEGlobal : lexerExpr ss
                                        ('A':('X':ss)) -> TANext : lexerExpr ss
                                        ('A':ss) -> TAll : lexerExpr ss
                                        ('E':ss) -> TExists : lexerExpr ss
                                        otherwise      -> case span isAlphaNum cs of
                                                               (v, rest) -> if v /= "" then (TPr v) : lexerExpr rest else [TErrE]
                                                               otherwise -> [TErrE]
}