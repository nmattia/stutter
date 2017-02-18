import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.Attoparsec.Text   (parseOnly)
import Data.Conduit
import Data.Monoid
import Data.List

import qualified Data.Conduit.Combinators as CL
import qualified Data.Text                as T
import qualified Options.Applicative      as Opts

import Stutter.Parser   (parseGroup)
import Stutter.Producer (ProducerGroup_, cardinality, prepareStdin, produceGroup)

data Options = Options
  { showCardinality :: Bool
  , showIntermediate :: Bool
  , producerGroupExpr :: String
  }

type ProducerGroup = ProducerGroup_ ()

parseCardinality :: Opts.Parser Bool
parseCardinality =
    Opts.switch
      ( Opts.long "size"
     <> Opts.long "cardinality"
     <> Opts.short 's'
     <> Opts.help "Just show output size"
      )

debug :: Opts.Parser Bool
debug =
    Opts.switch
      ( Opts.long "debug"
     <> Opts.short 'd'
     <> Opts.help "Just print parser output (mostly for debug purposes)"
      )

parseProducerGroup :: Opts.Parser String
parseProducerGroup =
    Opts.strArgument
      ( Opts.metavar "EXPR" )

parseOpts :: Opts.Parser Options
parseOpts = Options <$> parseCardinality <*> debug <*> parseProducerGroup

withProducerGroup :: String -> (ProducerGroup -> IO a) -> IO a
withProducerGroup str f =
    case parseOnly parseGroup $ T.pack str of
      Left err -> error $ intercalate " "
        [ "Could not parse producer group:", str
        , "Reason: ", err
        ]
      Right g -> f g

main :: IO ()
main = do
    a <- Opts.execParser opts
    if showIntermediate a
    then withProducerGroup (producerGroupExpr a) $ \g -> do
      print g
    else (
      if showCardinality a
      then withProducerGroup (producerGroupExpr a) $ \g -> do
        case cardinality g of
          Nothing -> putStrLn "?"
          Just x -> print x
      else withProducerGroup (producerGroupExpr a) $ \g -> do
        g' <- prepareStdin g
        runConduitRes
          $ produceGroup g'
         .| CL.mapM_ (liftIO . putStrLn . T.unpack)
       )
    where
    opts = Opts.info (parseOpts <**> Opts.helper)
      ( Opts.header "stutter - a string generator" )
