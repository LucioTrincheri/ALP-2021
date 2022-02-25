module Eval where

import           Common
import           Monads
--import           PrettierPrinter
import           Data.Maybe
import           Data.Foldable
import           Data.List
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Control.Monad                  ( liftM
                                                , ap
                                                )
import Text.Read (look)

import           Data.Tuple
import Data.Tree (flatten)

-- pprinter para errores y ver si falta algun error

type Env = ([State], [Transition], [Valuation])

-- Entorno nulo
initEnv :: Env
initEnv = ([],[],[])

newtype StateError a = StateError { runStateError :: Env -> (Either Error a, Env) }

instance Monad StateError where
  return x = StateError (\s -> (Right x, s))
  m >>= f  = StateError (\s ->
                    let (ev, s') = runStateError m s
                    in case ev of
                      Left e  -> (Left e, s')
                      Right v -> runStateError (f v) s')

instance MonadError StateError where
  throw e = StateError (\s -> (Left e, s))

getMissingStateTrans :: [State] -> [(State, State)] -> [State]
getMissingStateTrans s values = let unz = unzip values in (union s (fst unz) \\ s) ++ (union s (snd unz) \\ s)

getMissingStateVals :: [State] -> [(State, State)] -> [State]
getMissingStateVals s values = let unz = unzip values in (union s (snd unz) \\ s)

instance MonadState StateError where
  lookforStates = StateError (\z@(s, _, _) -> (Right s, z))

  lookforTransitions v = StateError (\z@(_, t, _) -> (Right (foldl (\xs (o, d) -> if v == o then d:xs else xs) [] t), z))
  
  lookforValuations v = StateError (\z@(_, _, r) -> (Right (foldl (\xs (o, d) -> if v == o then d:xs else xs) [] r), z))

  updateStates values = StateError (\(s, t, v) -> (Right (), (nub (s ++ values), t, v)))
   
  updateTransitions values = StateError (\z@(s, t, v) -> let missing = getMissingStateTrans s values in if null missing then (Right (), (s, nub (t ++ values), v)) else (Left (UndefState (head missing)), z))
  
  updateValuations values = StateError (\z@(s, t, v) -> let missing = getMissingStateVals s values in if null missing then (Right (), (s, nub (t ++ values), v)) else (Left (UndefState (head missing)), z))

-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

eval :: Comm -> Env -> (Either Error String, Env)
eval comm env = runStateError (evalComm comm) env


evalComm :: (MonadState m, MonadError m) => Comm -> m String
evalComm (CTL exp) = do x <- evalExp exp -- Ver que hacer con el string
                        return (show x)
evalComm (States states) = do updateStates states
                              return "" 
evalComm (Valuations valuations) = do updateValuations valuations
                                      return ""
evalComm (Transitions transitions) = do updateTransitions transitions
                                        return ""


-- evalExp es la implementacion del algoritmo SAT-solver
evalExp :: (MonadState m, MonadError m) => CTL -> m [State]
evalExp (Prop s) = do lookforValuations s
evalExp Bottom = do return [] 
evalExp (Not s) = do sE <- evalExp s
                     states <- lookforStates
                     return (states \\ sE)
evalExp (And s p) = do sE <- evalExp s
                       sP <- evalExp p
                       return (intersect sE sP)
evalExp (Or s p) = do sE <- evalExp s
                      sP <- evalExp p
                      return (union sE sP)
evalExp (EX s) = do sE <- evalExp s
                    existsNext sE
evalExp (AX s) = do sE <- evalExp s
                    allNext sE
evalExp (EU s p) = do sE <- evalExp s
                      sP <- evalExp p
                      existsUntil sE sP
evalExp (AU s p) = do sE <- evalExp s
                      sP <- evalExp p
                      allUntil sE sP
evalExp Top = evalExp (Not Bottom)
evalExp (Then s p) = evalExp (Or (Not s) p)
evalExp (EF s) = evalExp (EU Top s)
evalExp (AF s) = evalExp (AU Top s)
evalExp (EG s) = evalExp (Not (AF (Not s)))
evalExp (AG s) = evalExp (Not (EF (Not s)))


-- Dado un conjunto de estados que satisfacen la formula, retorna cuales de ellos cumplen EX
existsNext :: (MonadState m, MonadError m) => [State] -> m [State]
existsNext sE = do states <- lookforStates
                   x <- mapM lookforTransitions states 
                   let stateAndTrans = zip states x -- [(a,[a])]
                   let transCond = filter (\(originalState, trans) -> trans /= (trans \\ sE)) stateAndTrans
                   let statesThatCond = map fst transCond
                   return statesThatCond

-- Dado un conjunto de estados que satisfacen la formula, retorna cuales de ellos cumplen AX
allNext :: (MonadState m, MonadError m) => [State] -> m [State]
allNext sE = do states <- lookforStates
                x <- mapM lookforTransitions states 
                let stateAndTrans = zip states x -- [(a,[a])]
                let transCond = filter (\(originalState, trans) -> null (trans \\ sE)) stateAndTrans
                let statesThatCond = map fst transCond
                return statesThatCond

existsUntil :: (MonadState m, MonadError m) => [State] -> [State] -> m [State]
existsUntil sE sP = do existsN <- existsNext sP
                       let sR = union sP (intersect sE existsN)
                       if sP /= sR then existsUntil sE sR else return sP

allUntil :: (MonadState m, MonadError m) => [State] -> [State] -> m [State]
allUntil sE sP = do allN <- allNext sP
                    let sR = union sP (intersect sE allN)
                    if sP /= sR then allUntil sE sR else return sP