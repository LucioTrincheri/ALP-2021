module Common where
import Data.Set

type Prop = String
type State = String

type Transition = (State, State)
type Valuation = (Prop, State)

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

data Comm = CTL CTL | States [State] | Valuations [Valuation] | Transitions [Transition] deriving (Show)

data Error = OperOverEmpty | UndefState deriving (Eq, Show)