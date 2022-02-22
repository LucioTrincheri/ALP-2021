module Eval where

import           Common
import           Monads
--import           PrettierPrinter
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Tuple
import           Data.List
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Control.Monad                  ( liftM
                                                , ap
                                                )

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
  lookforVar v = StateError (\s ->
                let x = M.lookup v (snd s)
                in case x of
                  Nothing      -> (Left (UndefVar v), s)
                  Just rdyList -> (Right rdyList, s))
  lookforFun v = StateError (\s ->
                let x = M.lookup v (fst s)
                in case x of
                  Nothing      -> (Left (UndefFun v), s)
                  Just funList -> (Right funList, s))
  updateVar v i = StateError (\s -> (Right (), ((fst s), M.insert v i (snd s))))
  updateFun v i = StateError (\s -> (Right (), (M.insert v i (fst s), (snd s))))

-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

eval :: Comm -> Env -> (Either Error String, Env)
eval comm env = runStateError (evalComm comm) env


{-
eval :: SModel -> Set State
eval mdl = fst $ runState (sat (sctlExpr mdl)) mdl

sat :: MonadState m => CTL -> m (Set State)
sat Bottom = return empty
sat (Atomic str) = do vals <- getVals                   
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
-}