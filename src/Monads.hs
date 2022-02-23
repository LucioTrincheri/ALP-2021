module Monads where

import           Common

-- Clase para representar mónadas con estado de variables

class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookforStates :: m [State] -- Devuelve todos los estados
    lookforTransicion :: m [Transition]
    lookforValuation :: m [Valuation]
    -- Cambia el valor de una variable
    updateStates :: [State] -> m ()
    updateTransitions :: [Transition] -> m ()
    updateValuations :: [Valuation] -> m ()

-- Clase para representar mónadas que lanzan errores

class Monad m => MonadError m where
    -- Lanza un error
    throw :: Error -> m a