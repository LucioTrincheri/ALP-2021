module Main where

import System.Environment (getArgs)
import           Control.Monad.Except
import           Data.Char
import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( print )
import           System.Console.Haskeline
import qualified Control.Monad.Catch           as MC
import           System.Environment
import           System.IO               hiding ( print )
import           Text.PrettyPrint.HughesPJ      ( render
                                                , text
                                                )
import Control.Monad.IO.Class
import Parse
import Common
--import Eval
import Data.Set
import Data.List
--import PPrint

parseModel :: String -> Comm
parseModel contents = func $ lexerComm contents

main :: IO ()
main = do putChar 'c'

