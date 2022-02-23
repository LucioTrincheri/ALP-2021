module Eval where

import           Common
import           Monads
--import           PrettierPrinter
import           Data.Maybe
import           Data.Tuple
import           Data.List
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Control.Monad                  ( liftM
                                                , ap
                                                )
import Text.Read (look)

-- The model itself
data Model = Model {
    states       :: [State],
    transitions  :: [Transition],
    valuations   :: [Valuation] 
} deriving Show

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


instance MonadState StateError where
  lookforStates v = StateError (\z@(s, _, _) -> (Right s, z))
  lookforTransitions v = StateError (\s ->
                let x = M.lookup v (fst s)
                in case x of
                  Nothing      -> (Left (UndefFun v), s)
                  Just funList -> (Right funList, s))
  lookforValuations v = StateError (\s ->
                let x = M.lookup v (fst s)
                in case x of
                  Nothing      -> (Left (UndefFun v), s)
                  Just funList -> (Right funList, s))

  updateStates values = StateError (\(s, t, v) -> (Right (), (uniq (s ++ values), t, v)))
  
  updateTransitions values = StateError (\z@(s, t, v) -> if foldl (\x (o, d) -> (elem o s) && (elem d s) && x) True values then (Right (), (s, uniq (t ++ values), v)) else (Left (UndefState), z))
  
  updateValuations values = StateError (\z@(s, t, v) -> if foldl (\x (o, d) -> (elem d s) && x) True values then (Right (), (s, t, uniq (v ++ values))) else (Left (UndefState), z))

-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

eval :: Comm -> Env -> (Either Error String, Env)
eval comm env = runStateError (evalComm comm) env


evalComm :: (MonadState m, MonadError m) => Comm -> m String
evalComm (CTL exp) = evalExp exp -- Ver que hacer con el string
evalComm (States states) = do updateState states
                              return "" 
evalComm (Valuations valuations) = do updateValuations valuations
                                      return ""
evalComm (Transitions transitions) = do updateTransitions transitions
                                        return ""

{-
data CTL =  Prop Prop
          | Bottom
          | Not CTL
          | And CTL CTL
          | Or CTL CTL
          | AX CTL        -- Para todo siguiente
          | EX CTL        -- Existe siguiente
          | AU CTL CTL    -- For All Until
          | EU CTL CTL    -- Exists Until
-- Expresiones derivadas (se transforman en atomicas en el evaluador)
-- Esto se realiza para que la abstracción sea de bajo nivel.
          | Top           -- Not Bottom
          | AF CTL        -- AU Top ctl
          | EF CTL        -- EU Top ctl
          | AG CTL        -- Not (EF (Not ctl))
          | EG CTL        -- Not (AF (Not ctl))
          | Then CTL CTL  -- Or (Not ctl1) ctl2
          deriving Show
-}


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
evalExp (EX s) = do states <- lookforStates
                    sE <- evalExp s
                    return foldl (\xs x -> do {trans <- lookforTransitions x ; if trans /= (trans \\ sE) then x:xs else xs}) [] states
evalExp (AX s) = do states <- lookforStates
                    sE <- evalExp s
                    return foldl (\xs x -> do {trans <- lookforTransitions x ; if isEmpty (trans \\ sE) then x:xs else xs}) [] states
evalExp (EU s p) = 
evalExp (AU s p) = do states <- lookforStates
                      sE <- evalExp s
                      return foldl (\xs x -> do {vals <- lookforValuation x ; if vals /= (vals \\ sE) then x:xs else xs}) [] states
evalExp Top = evalExp (Not Bottom)
evalExp (Then s p) = evalExp (Or (Not s) p)
evalExp (EF s) = evalExp (EU Top s)
evalExp (AF s) = evalExp (AU Top s)
evalExp (EG s) = evalExp (Not (AF (Not s)))
evalExp (AG s) = evalExp (Not (EF (Not s)))



existsNext :: (MonadState m, MonadError m) => CTL -> m [State]
existsNext s = do states <- lookforStates
                  sE <- evalExp s
                  return foldl (\xs x -> do {trans <- lookforTransitions x ; if trans /= (trans \\ sE) then x:xs else xs}) [] states

existsUntil :: (MonadState m, MonadError m) => CTL -> CTL -> m [State]
existsUntil s p = do sE <- evalExp s
                     sP <- evalExp p
                     do existsN <- existsNext p -- puede ser sP
                        sR <- union (sP (intersect sE existsN)) 
                        if sP /= sR then existsUntil sE sR else return sP -- Problema sE y sR son [State] pero necesitan ser CTL. Pasar todo a [State] en vez de CTL llevando el do sE y sP a la llama de la funcion EU o EX porque sino no se puede recursionar
sat (EX ctl) = do res <- sat ctl
                  preExists res

sat (EU ctl ctl') = do res <- sat ctl
                       res' <- sat ctl'
                       existsUntil res res'

preExists :: MonadState m => Set State -> m (Set State)
preExists xs = do sts <- getStates
                  rels <- getRels
                  let pre = go (toList sts) (toList xs) rels 
                  return $ fromList pre
              where go [] _  _= []
                    go (s:sts) xs rels = let l' = go sts xs rels
                                         in if any (\s'-> (s, s') `elem` rels ) xs then s:l'
                                            else l'  

existsUntil ::  MonadState m => Set State -> Set State -> m (Set State)
existsUntil res1 res2 = do preE <- preExists res2
                           let res = res2 `union` (res1 `intersection` preE)
                           if res2 /= res then existsUntil res1 res
                           else return res2













sat :: MonadState m => CTL -> m (Set State)
sat Bottom = return empty
sat (Prop str) = do vals <- lookforValuations str                    
                      return $ getMatchedStates str vals
sat (Not ctl) = do res <- sat ctl
                   sts <- getStates
                   return $ sts \\ res  
sat (And ctl ctl') = do res <- sat ctl
                        res' <- sat ctl'
                        return $ res `intersection` res'
sat (Or ctl ctl') = do res <- sat ctl
                       res' <- sat ctl'
                       return $ res `union` res'
sat (EX ctl) = do res <- sat ctl
                  preExists res
sat (AX ctl) = do res <- sat ctl
                  preForAll res                                     
sat (EU ctl ctl') = do res <- sat ctl
                       res' <- sat ctl'
                       existsUntil res res'
sat (AU ctl ctl') = do res <- sat ctl
                       res' <- sat ctl'
                       forAllUntil res res'                    

-- Returns the states where an atomic is valid
getMatchedStates :: Atomic -> [Valuation] -> Set State
getMatchedStates _ [] = empty
getMatchedStates at ((at', states):xs) = if at == at' then fromList states
                                         else getMatchedStates at xs


preExists :: MonadState m => Set State -> m (Set State)
preExists xs = do sts <- getStates
                  rels <- getRels
                  let pre = go (toList sts) (toList xs) rels 
                  return $ fromList pre
              where go [] _  _= []
                    go (s:sts) xs rels = let l' = go sts xs rels
                                         in if any (\s'-> (s, s') `elem` rels ) xs then s:l'
                                            else l'  

-- Usamos la igualdad: preForAll(Y) = S - preExists(S - Y)
preForAll :: MonadState m => Set State -> m (Set State)
preForAll xs = do sts <- getStates
                  pre <- preExists (sts \\ xs)
                  return $ sts \\ pre

inev :: MonadState m => Set State -> m (Set State)
inev xs = do xs' <- preForAll xs  
             let ys = xs `union` xs'
             if xs /= ys then inev ys 
             else return xs

existsUntil ::  MonadState m => Set State -> Set State -> m (Set State)
existsUntil res1 res2 = do preE <- preExists res2
                           let res = res2 `union` (res1 `intersection` preE)
                           if res2 /= res then existsUntil res1 res
                           else return res2

forAllUntil ::  MonadState m => Set State -> Set State -> m (Set State)
forAllUntil res1 res2 = do preE <- preForAll res2
                           let res = res2 `union` (res1 `intersection` preE)
                           if res2 /= res then forAllUntil res1 res
                           else return res2
