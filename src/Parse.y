-- Se puede ingresar por consola o por archivo, cada linea se parsea por separado por lo cual se puede ingresar
-- mas de una linea repetida, ej 2 States etc. Luego de ingresar una expresion, se calcula la tabla de verdad y se puede seguir trabajando. 
{-
States [s1, s2]

States [s3]

States [4,5,6]

Expresion ... Execute

States []

M = (S, T, V)
States stateList ([string, string ...])
Transitions trasitionList ([(string, string), (string, string) ...])
Valuations valuationList [(state, variable), (state, variable2)]
Expression exp



Expresion ..


-}


{ 
module Parse where

import Common
import Data.Char
import Data.List
}

%name func 
%tokentype { Token } 
%error { parseError }

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

%left '&' '|' 
%left '->' 
%nonassoc '!' AF EF AG EG AX EX AE E A

%%

fields :: { Comm }
fields :  States      sts     { States $2 }  
       |  Transitions trns    { Transitions $2 } 
       |  Valuations  vals    { Valuations $2 } 
       |  Exp         ctl     { CTL $2 }        

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

            deriving Show

parseError :: [Token] -> a
parseError _ = error "Parse error"

-- Main lexer
lexerComm :: String -> [Token]
lexerComm [] = []
lexerComm cs@(c:cc) | isSpace c = lexerComm cc
                    | otherwise = case span isAlphaNum cs of
                                       ("STATES", rest) -> TStates : lexerModel rest
                                       ("TRANSITIONS", rest) -> TTransitions : lexerModel rest
                                       ("VALUATIONS", rest) -> TValuations : lexerModel rest
                                       ("CTLEXP", rest) -> TExp : lexerExpr rest
                                       (v, rest) -> parseError []

lexerModel [] = []
lexerModel cs@(c:cc) | isSpace c = lexerModel cc
                     | c == '['  = TBracketL : lexerModel cc
                     | c == ']'  = TBracketR : lexerModel cc
                     | c == ','  = TComma : lexerModel cc
                     | c == '('  = TParenLeft: lexerModel cc
                     | c == ')'  = TParenRight: lexerModel cc
                     | c == ')'  = TParenRight: lexerModel cc
                     | otherwise = case span isAlphaNum cs of
                                        (v, rest) -> TString v : lexerModel rest
                                        otherwise -> parseError []

lexerExpr [] = []
lexerExpr cs@(c:cc)  | isSpace c = lexerExpr cc
                     | c == '!'  = TNot : lexerExpr cc
                     | c == '&'  = TAnd : lexerExpr cc
                     | c == '|'  = TOr : lexerExpr cc
                     | c == '['  = TBracketL : lexerExpr cc
                     | c == ']'  = TBracketR : lexerExpr cc
                     | c == '('  = TParenLeft : lexerExpr cc
                     | c == ')'  = TParenRight : lexerExpr cc
                     | c == 'A'  = TAll : lexerExpr cc
                     | c == 'E'  = TExists : lexerExpr cc
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
                                        otherwise      -> case span isAlphaNum cs of
                                                               (v, rest) -> (TPr v) : lexerExpr rest
                                                               otherwise -> parseError []
}