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
--import           Text.PrettyPrint.HughesPJ      ( render
--                                                , text
--                                                )
import Control.Monad.IO.Class
import Parse
import Eval
import Common

-- Funcion para imprimir errores de falta de estados
showError :: Error -> String
showError (UndefState v) = "Error: Missing \"State\": " ++ v

-- rstrip: Remueve los caracteres de fin de linea y espacios al final de un string
rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

-- Funcion de printeo en consola. Solo esta es necesaria ya que las listas y los strings de los estados derivan de la clase Show
ctlPrint :: String -> String -> IO ()
ctlPrint line ss = putStrLn ("Estados del modelo que satisfacen \"" ++ (Data.List.drop 7 line) ++ "\" = " ++ ss)

-- El main se encarga de la existencia y buena lectura del archivo. El parseo y evaluacion se delega a processFile
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
                 do putStrLn "ERROR: Extension invalida. La extension debe ser del tipo .ctl"
          else 
            do putStrLn "ERROR: Falta de archivo"


-- Funcion que dado un archivo y el enviroment, se encarga de procesar las lineas del archivo 
processFile :: [String] -> Env -> IO ()
processFile [] _ = return ()
processFile (x:xs) env = case x of
                          -- Si la linea es vacia recursiono
                          ""   -> processFile xs env
                          -- En caso de que se lea una linea
                          line -> do case parseModel (rstrip line) of
                                       -- Si el parser detecta un error, lo muestro en pantalla y finalizo el programa
                                       (ParseError error) -> do putStr error
                                                                return ()
                                       -- Exit del programa
                                       Exit -> return ()
                                       -- En caso de parseo valido
                                       x    -> do case eval x env of
                                                   -- Si la evaluacion llega a un error, lo muestro por pantalla y finalizo el programa
                                                   (Left err, env') -> do putStrLn (showError err)
                                                   -- En caso de evaluacion valida
                                                   (Right s, env') -> do case s of
                                                                             -- Si no retorna nada (cuando se añade al estado) recursiono
                                                                             "" -> processFile xs env'
                                                                             -- Si devuelve algo (cuando es una formula) lo imprimo por pantalla y recursiono
                                                                             ss -> do ctlPrint line ss
                                                                                      processFile xs env'