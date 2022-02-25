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
import           System.IO               --hiding ( print )
import           Text.PrettyPrint.HughesPJ      ( render
                                                , text
                                                )
import Control.Monad.IO.Class
import Parse

import Eval
--import Eval
import Common

--import PPrint

showError :: Error -> String
showError (UndefState v) = "Error: Missing \"State\": " ++ v

rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

ctlPrint :: String -> String -> IO ()
ctlPrint line ss = putStrLn ("Estados del modelo que satisfacen \"" ++ (Data.List.drop 7 line) ++ "\" = " ++ ss)

main :: IO ()
main = do args <- getArgs
          let initE = initEnv 
          if args /= [] then
            do let filename = head args
               if ".ctl" `isSuffixOf` filename then
                do contents <- readFile filename
                   let fileLines = lines contents
                   processFile fileLines initE
               else 
                 do putStrLn "ERROR: The file extension is not valid, make sure you are giving a .ctl file"
          else 
            do putStrLn "ERROR: No file"



processFile [] env = putStr "Archivo parseado correctamente\n"
processFile (x:xs) env = case x of
                          ""   -> processFile xs env
                          -- Se elimina el \n, por eso init
                          line -> do case parseModel (rstrip line) of
                                       (ParseError error) -> do putStr error
                                                                return ()
                                       Exit -> return ()
                                       x    -> do case eval x env of
                                                   (Left err, env') -> do putStrLn (showError err)
                                                   (Right s, env') -> do case s of
                                                                             "" -> processFile xs env'
                                                                             ss -> do ctlPrint line ss
                                                                                      processFile xs env'