module Common where

type Prop = String
type State = String

type Transition = (State, State)
type Valuation = (Prop, State)

type Env = ([State], [Transition], [Valuation])

-- Entorno nulo
initEnv :: Env
initEnv = ([],[],[])

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

data Comm = CTL CTL | States [State] | Valuations [Valuation] | Transitions [Transition] | Exit | ParseError String deriving (Show)

data Error = UndefState State deriving (Eq, Show)

-- Codigo para manejar errores de parseo. Proveniente de https://www.haskell.org/happy/doc/html/sec-monads.html#sec-exception
data E a = Ok a | Failed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k = 
   case m of 
       Ok a -> k a
       Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k = 
   case m of
      Ok a -> Ok a
      Failed e -> k e