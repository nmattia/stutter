{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Stutter.Producer where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit
import Data.Conduit.Internal (zipSources)
import System.IO (stdin)

import qualified Data.ByteString.Lazy     as BL
import qualified Data.Conduit.Binary      as CB
import qualified Data.Conduit.Combinators as CL
import qualified Data.Text                as T

data Range
  = IntRange (Int, Int)
  | CharRange (Char, Char)
  deriving (Eq, Show)

data ProducerGroup_ a
  = PSum (ProducerGroup_ a) (ProducerGroup_ a)
  | PProduct (ProducerGroup_ a) (ProducerGroup_ a)
  | PZip (ProducerGroup_ a) (ProducerGroup_ a)
  | PRepeat (ProducerGroup_ a)
  | PRanges [Range]
  | PFile FilePath
  | PStdin a
  | PText T.Text
  deriving (Eq, Show, Functor, Foldable, Traversable)

type ProducerGroup = ProducerGroup_ BL.ByteString

prepareStdin :: ProducerGroup_ () -> IO (ProducerGroup_ BL.ByteString)
prepareStdin p = evalStateT (traverse f p) Nothing
  where
    f () = get >>= \case
      Just bs -> return bs
      Nothing -> do
        bs <- liftIO $ BL.hGetContents stdin
        modify (const $ Just bs)
        return bs

cardinality :: ProducerGroup_ a -> Maybe Int
cardinality (PSum p p') = (+) <$> cardinality p <*> cardinality p'
cardinality (PProduct p p') = (*) <$> cardinality p <*> cardinality p'
cardinality (PZip p p') = min <$> cardinality p <*> cardinality p'
cardinality (PRepeat _) = Nothing
cardinality (PRanges rs) = pure $ sum $ map rangeCardinality rs
  where
    rangeCardinality (IntRange (a,z)) = length [a..z]
    rangeCardinality (CharRange (a,z)) = length [a..z]
cardinality PFile{} = Nothing
cardinality PStdin{} = Nothing
cardinality PText{} = pure 1

produceRanges :: (Monad m) => [Range] -> ConduitT () T.Text m ()
produceRanges = CL.yieldMany
              . concat
              . map rangeToList
  where
    rangeToList (IntRange (a,z))  = tshow <$> [a..z]
    rangeToList (CharRange (a,z)) = T.pack . (:[]) <$> [a..z]
    tshow = T.pack . show

produceGroup
  :: (MonadIO m, MonadResource m, MonadThrow m)
  => ProducerGroup
  -> ConduitT () T.Text m ()
produceGroup (PRanges rs) = produceRanges rs
produceGroup (PText t) = yield t
produceGroup (PProduct g g') =
    produceGroup g
    .| awaitForever (\t -> forever (yield ())
    .| produceGroup g'
    .| awaitForever (\t' -> yield (t <> t')))
produceGroup (PSum g g') = produceGroup g >> produceGroup g'
produceGroup (PZip g g') =
    zipSources (produceGroup g) (produceGroup g')
    .| CL.map (uncurry (<>))
produceGroup (PRepeat g) = forever $ produceGroup g
produceGroup (PFile f) =
    CB.sourceFile f
    .| CB.lines
    .| CL.decodeUtf8
produceGroup (PStdin bs) =
    CB.sourceLbs bs
    .| CB.lines
    .| CL.decodeUtf8
