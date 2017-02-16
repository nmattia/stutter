import Control.Monad.IO.Class (liftIO)
import Data.Attoparsec.Text   (parseOnly)
import Data.Conduit
import System.Environment     (getArgs)

import qualified Data.Conduit.Combinators as CL
import qualified Data.Text                as T

import Stutter.Parser   (parseGroup)
import Stutter.Producer (cardinality, prepareStdin, produceGroup)

main :: IO ()
main = do
    a <- getArgs
    case a of
      ["--cardinality", a'] -> case parseOnly parseGroup $ T.pack a' of
          Left str -> error str
          Right g -> case cardinality g of
            Nothing -> putStrLn "?"
            Just x  -> print x
      [a'] -> case parseOnly parseGroup $ T.pack a' of
        Left str -> error str
        Right g -> do
          g' <- prepareStdin g
          runConduitRes $ produceGroup g' .| CL.mapM_ (liftIO . putStrLn . T.unpack)
      ["--", a'] -> case parseOnly parseGroup $ T.pack a' of
        Left str -> error str
        Right g -> do
          g' <- prepareStdin g
          runConduitRes $ produceGroup g' .| CL.mapM_ (liftIO . putStrLn . T.unpack)
      _ -> error "bad"
