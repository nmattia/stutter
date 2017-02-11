import Control.Monad.IO.Class (liftIO)
import Data.Attoparsec.Text   (parseOnly)
import Data.Conduit
import System.Environment     (getArgs)

import qualified Data.Conduit.Combinators as CL
import qualified Data.Text                as T

import Stutter.Parser   (parseGroup)
import Stutter.Producer (produceGroup)

main :: IO ()
main = do
    a <- getArgs
    case a of
      [a'] -> case parseOnly parseGroup $ T.pack a' of
        Left str -> error str
        Right g -> runConduitRes $ produceGroup g =$ CL.mapM_ (liftIO . putStrLn . T.unpack)
      _ -> error "bad"
