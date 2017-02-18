import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.Attoparsec.Text   (parseOnly, endOfInput)
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
  , allowSloppyParse :: Bool
  , disableNewLines :: Bool
  , producerGroupExpr :: String
  }

type ProducerGroup = ProducerGroup_ ()

parseCardinality :: Opts.Parser Bool
parseCardinality =
    Opts.switch
      ( Opts.long "size"
     <> Opts.long "length"
     <> Opts.short 'l'
     <> Opts.help "Just show output size"
      )

debug :: Opts.Parser Bool
debug =
    Opts.switch
      ( Opts.long "debug"
     <> Opts.short 'd'
     <> Opts.help "Just print parser output (mostly for debug purposes)"
      )

allowSloppy :: Opts.Parser Bool
allowSloppy =
    Opts.switch
      ( Opts.long "sloppy"
     <> Opts.short 's'
     <> Opts.help "Allow parser to parse partially"
      )

noLn :: Opts.Parser Bool
noLn =
    Opts.switch
      ( Opts.long "no-newlines"
     <> Opts.short 'n'
     <> Opts.help "Do not print newlines between outputs"
      )

parseProducerGroup :: Opts.Parser String
parseProducerGroup =
    Opts.strArgument
      ( Opts.metavar "EXPR" )

parseOpts :: Opts.Parser Options
parseOpts =
  Options
    <$> parseCardinality
    <*> debug
    <*> allowSloppy
    <*> noLn
    <*> parseProducerGroup

withProducerGroup :: Options -> String -> (ProducerGroup -> IO a) -> IO a
withProducerGroup opts str f =
    case parseOnly parser $ T.pack str of
      Left err -> error $ intercalate " "
        [ "Could not parse producer group:", str
        , "Reason: ", err
        ]
      Right g -> f g
  where
    -- If "sloppy" mode is enabled, allow partial parse. Otherwise, request end
    -- of input.
    parser =
      if allowSloppyParse opts
      then parseGroup
      else parseGroup <* endOfInput

main :: IO ()
main = do
    a <- Opts.execParser opts
    if showIntermediate a
    then withProducerGroup a (producerGroupExpr a) $ \g -> do
      print g
    else (
      if showCardinality a
      then withProducerGroup a (producerGroupExpr a) $ \g -> do
        case cardinality g of
          Nothing -> putStrLn "?"
          Just x -> print x
      else withProducerGroup a (producerGroupExpr a) $ \g -> do
        g' <- prepareStdin g
        let print' =
             if disableNewLines a
             then putStr
             else putStrLn
        runConduitRes
          $ produceGroup g'
         .| CL.mapM_ (liftIO . print' . T.unpack)
       )
    where
    opts = Opts.info (parseOpts <**> Opts.helper)
      ( Opts.header "stutter - a string generator" )
