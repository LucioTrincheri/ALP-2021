module Eval1
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
lookfor :: Variable -> State -> Int
lookfor v s = s M.! v

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm (Skip) s = (Skip :!: s)
stepComm (Let x y) s = let (n :!: s') = evalExp y s in (Skip :!: update x n s')
stepComm (Seq Skip n) s = (n :!: s)
stepComm (Seq s1 s2) s = let (e' :!: s') = stepComm s1 s in (Seq e' s2 :!: s')
stepComm (IfThenElse x y z) s = let (n :!: s') = evalExp x s 
                                in if n then (y :!: s') else (z :!: s')
stepComm (IfThen x y) s = let (n :!: s') = evalExp x s
                          in if n then (y :!: s') else (Skip :!: s')
stepComm (Repeat x b) s = ((Seq x (IfThenElse b Skip (Repeat x b))) :!: s)


evalBinary x y s = (v1, v2, s'') where 
                                 (v1 :!: s') = evalExp x s
                                 (v2 :!: s'') = evalExp y s'

-- Evalua una expresion
evalExp :: Exp a -> State -> Pair a State
--Int
evalExp (Const n) s = (n :!: s)
evalExp (Var n) s = (lookfor n s :!: s)
evalExp (UMinus n) s = ((-v) :!: s') where 
                                     (v :!: s') = evalExp n s

evalExp (Plus x y) s = (v1 + v2 :!: s'') where 
                                         (v1, v2, s'') = evalBinary x y s
evalExp (Minus x y) s = (v1 - v2 :!: s'') where 
                                          (v1, v2, s'') = evalBinary x y s
evalExp (Times x y) s = (v1 * v2 :!: s'') where 
                                          (v1, v2, s'') = evalBinary x y s
evalExp (Div x y) s = (div v1 v2 :!: s'') where
                                          (v1, v2, s'') = evalBinary x y s
evalExp (EAssgn xs y) s = (v :!: update xs v s') where
                                                 (v :!: s') = evalExp y s

-- Bool
evalExp BTrue s = (True :!: s) 
evalExp BFalse s = (False :!: s)
evalExp (Lt x y) s = (v1 < v2 :!: s'') where 
                                       (v1, v2, s'') = evalBinary x y s

evalExp (Gt x y) s = (v1 > v2 :!: s'') where 
                                       (v1, v2, s'') = evalBinary x y s

evalExp (Eq x y) s = ((v1 == v2) :!: s'') where 
                                        (v1, v2, s'') = evalBinary x y s
evalExp (NEq x y) s = (v1 /= v2 :!: s'') where 
                                         (v1, v2, s'') = evalBinary x y s
evalExp (And x y) s = ((v1 && v2) :!: s'') where 
                                           (v1, v2, s'') = evalBinary x y s
evalExp (Or x y) s = ((v1 || v2) :!: s'') where 
                                          (v1, v2, s'') = evalBinary x y s
evalExp (Not x) s = (not v1 :!: s') where 
                                    (v1 :!: s') = evalExp x s