module Monads where

import           Common

-- Clase para representar mónadas con estado de variables

class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookforState :: m [State]
    lookforTransicion :: m [Transition]
    lookforValuation :: m [Valuation]
    -- Cambia el valor de una variable
    updateState :: [State] -> m ()
    updateTransicion :: [Transition] -> m ()
    updateValuation :: [Valuation] -> m ()

-- Clase para representar mónadas que lanzan errores

class Monad m => MonadError m where
    -- Lanza un error
    throw :: Error -> m a