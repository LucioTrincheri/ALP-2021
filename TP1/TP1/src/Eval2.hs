module Eval2
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado nulo
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case M.lookup v s of
                  Just n -> Right n
                  Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm (Skip) s = Right (Skip :!: s)
stepComm (Let x y) s = case evalExp y s of
                            Right (n :!: s') -> Right (Skip :!: update x n s')
                            Left e -> Left e
stepComm (Seq Skip n) s = Right (n :!: s)
stepComm (Seq s1 s2) s = case stepComm s1 s of
                              Right (e' :!: s') -> Right (Seq e' s2 :!: s')
                              Left e -> Left e
stepComm (IfThenElse x y z) s = case evalExp x s of
                                     Right (True :!: s') -> Right (y :!: s')
                                     Right (False :!: s') -> Right (z :!: s')
                                     Left e -> Left e
stepComm (IfThen x y) s = case evalExp x s of
                               Right (True :!: s') -> Right (y :!: s')
                               Right (False :!: s') -> Right (Skip :!: s')
                               Left e -> Left e
stepComm (Repeat x b) s = Right ((Seq x (IfThenElse b Skip (Repeat x b))) :!: s)


evalBinary x y f s = case evalExp x s of
                          Right (v1 :!: s') -> case evalExp y s' of
                                                      Right (v2 :!: s'') -> Right (f v1 v2 :!: s'')
                                                      Left e -> Left e
                          Left e -> Left e

-- Evalua una expresion
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
-- Int
evalExp (Const n) e = Right (n :!: e)
evalExp (Var n) e = case lookfor n e of
                         Right x -> Right (x :!: e)
                         Left x -> Left x
evalExp (UMinus n) e = case evalExp n e of
                            Right (v :!: e') -> Right ((-v) :!: e')
                            Left x -> Left x
evalExp (EAssgn xs y) s = case evalExp y s of 
                              Right (v :!: s') -> Right (v :!: update xs v s')
                              Left e -> Left e
evalExp (ESeq s1 s2) s = case evalExp s1 s of 
                              Right (_ :!: s') -> evalExp s2 s'
                              Left e -> Left e
evalExp (Plus x y) s = evalBinary x y (+) s
evalExp (Minus x y) s = evalBinary x y (-) s
evalExp (Times x y) s = evalBinary x y (*) s
evalExp (Div x y) s = case evalExp x s of
                            Right (v1 :!: s') -> case evalExp y s' of
                                                      Right (0 :!: s'') -> Left DivByZero
                                                      Right (v2 :!: s'') -> Right (div v1 v2 :!: s'')
                                                      Left e -> Left e
                            Left e -> Left e  

-- Bool
evalExp BTrue s = Right (True :!: s) 
evalExp BFalse s = Right (False :!: s)
evalExp (Lt x y) s = evalBinary x y (<) s
evalExp (Gt x y) s = evalBinary x y (>) s
evalExp (Eq x y) s = evalBinary x y (==) s
evalExp (NEq x y) s = evalBinary x y (/=) s
evalExp (And x y) s = evalBinary x y (&&) s
evalExp (Or x y) s = evalBinary x y (||) s
evalExp (Not x) s = case evalExp x s of
                     Right (v1 :!: s') -> Right (not v1 :!: s')
                     Left e -> Left e