module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

-- Comentarios sobre el código dentro del informe.

----------------------------------------------
-- Sección 2  
-- Ejercicio 2: Conversión a términos localmente sin nombres
----------------------------------------------

stateUpdate :: String -> [(String, Int)] -> [(String, Int)]
stateUpdate abs state = (abs, 0) : (mapMaybe (\(x,y) -> if abs /= x then Just (x,y+1) else Nothing) state)

stateFind :: String -> [(String, Int)] -> Int
stateFind _ [] = -1
stateFind abs ((x,y):xs) | abs == x = y
                         | otherwise = stateFind abs xs

conversion :: LamTerm -> Term
conversion t = conversionAux t []

conversionAux :: LamTerm -> [(String, Int)] -> Term
conversionAux (App t1 t2) state = let e1 = conversionAux t1 state
                                      e2 = conversionAux t2 state
                                  in e1 :@: e2
conversionAux (Abs abs t) state = Lam (conversionAux t (stateUpdate abs state))
conversionAux (LVar abs) state = case (stateFind abs state) of
                                     -1 -> (Free (Global abs))
                                     n -> Bound n
-- Ejemplos
-- λx. x → λ0 = conversion (Abs "x" (LVar "x"))

-- λx. λy. λz. x → λλλ2 = conversion (Abs "x" (Abs "y" (Abs "z" (LVar "x"))))

-- λy.(λx y.x) y → λ(λλ1) 0 = conversion (Abs "y" (App (Abs "x" (Abs "y" (LVar "x"))) (LVar "y")))

-- λx. λy. x (λy. y x) → λλ1 (λ0 2) = conversion (Abs "x" (Abs "y" (App (LVar "x") (Abs "y" (App (LVar "y") (LVar "x"))))))

-------------------------------
-- Sección 3
-------------------------------

vapp :: Value -> Value -> Value
vapp (VLam x) z = x z
vapp (VNeutral x) z = VNeutral (NApp x z)

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv) = lEnv !! ii
eval' (Free name) (nEnv, _) = stateFindG name nEnv
eval' (a :@: b) e@(nEnv, lEnv) = vapp (eval' a e) (eval' b e)
eval' (Lam f) (nEnv, lEnv) = VLam (\x-> eval' f (nEnv, x:lEnv))

stateFindG :: Name -> NameEnv Value -> Value
stateFindG abs [] = VNeutral (NFree abs)
stateFindG abs ((x,y):xs) | abs == x = y
                          | otherwise = stateFindG abs xs

--data Term  = Bound Int
--           | Free Name
--           | Term :@: Term
--           | Lam Term
--        deriving (Show,Eq)
-- type NameEnv v = [(Name, v)]


--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

--data Value
--    =  VLam      (Value -> Value)
--    |  VNeutral  Neutral

quote :: Value -> Term
quote v = quoteAux v 0

-- Int: cantidad de variables frescas aplicadas hasta el momento. 
quoteAux :: Value -> Int -> Term
-- como el termino del lam puede ser otro lam debemos recurcionar aumentando el numero de variables frecas pues acabamos de usar la numero i.
quoteAux (VLam f) i = Lam (quoteAux (f (VNeutral (NFree (Quote i)))) (i+1))
quoteAux (VNeutral neu) i = (quoteAux' neu i)
                            
quoteAux' :: Neutral -> Int -> Term
quoteAux' (NFree (Quote k)) n = Bound (n-k-1)
quoteAux' (NFree  name) n = Free name
quoteAux' (NApp neu v) n = (quoteAux' neu n) :@: (quoteAux v n)
