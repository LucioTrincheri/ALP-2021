module Monads where

import           Common

-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    lookforStates :: m [State]                -- Devuelve todos los estados
    lookforTransitions :: State -> m [State]  -- Dado un estados, busca sus transiciones en un paso
    lookforValuations :: Prop -> m [State]    -- Dada una proposicion, busca los estados que la satisfagan 

    updateStates :: [State] -> m ()           -- Dada una lista de estados, actualiza los estados del env
    updateTransitions :: [Transition] -> m () -- Dada una lista de transiciones, actualiza las transiciones del env
    updateValuations :: [Valuation] -> m ()   -- Dada una lista de valuaciones, actualiza las valuaciones del env

-- Clase para representar mónadas que lanzan errores
class Monad m => MonadError m where
    -- Lanza un error
    throw :: Error -> m a