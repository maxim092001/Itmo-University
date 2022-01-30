module Main where
import Text.Megaparsec.Error
import System.Console.Haskeline
import HW3.Parser (parse)
import HW3.Evaluator (eval)
import HW3.Base
import Data.Void
import HW3.Pretty (prettyValue)
import HW3.Action
import Control.Monad.IO.Class
import Data.Set

main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "hi> "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do
                      exP <- getExternalPrint
                      let eitherIn = parse input
                      case eitherIn of
                        Left e -> liftIO $ (exP . show) e
                        Right e -> do
                                   r <- liftIO $ runHIO (eval e) (fromList [AllowTime, AllowRead, AllowWrite])
                                   liftIO $ either (exP . show) (exP . show . prettyValue) r
                      loop


