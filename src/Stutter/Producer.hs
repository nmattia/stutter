{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Stutter.Producer where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit
import Data.Monoid
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
  | PRanges [Range]
  | PFile FilePath
  | PStdin a
  | PText T.Text
  deriving (Eq, Show, Functor, Foldable, Traversable)

type ProducerGroup = ProducerGroup_ BL.ByteString

prepareStdin :: ProducerGroup_ () -> IO (ProducerGroup_ BL.ByteString)
prepareStdin = traverse f
  where
    f () = BL.hGetContents stdin

produceRanges :: (Monad m) => [Range] -> Producer m T.Text
produceRanges = CL.yieldMany
              . concat
              . map rangeToList
  where
    rangeToList (IntRange (a,z))  = tshow <$> [a..z]
    rangeToList (CharRange (a,z)) = T.pack . (:[]) <$> [a..z]
    tshow = T.pack . show

produceGroup
  :: (MonadIO m, MonadResource m)
  => ProducerGroup
  -> Producer m T.Text
produceGroup (PRanges rs)      = produceRanges rs
produceGroup (PText t)         = yield t
produceGroup (PProduct g g')   = produceGroup g
                              $= awaitForever (\t -> produceGroup g'
                             =$= CL.map (t <>))
produceGroup (PSum g g')       = produceGroup g >> produceGroup g'
produceGroup (PFile f)         = CB.sourceFile f
                              $= CB.lines
                             =$= CL.decodeUtf8
produceGroup (PStdin bs)       = CB.sourceLbs bs
                              $= CB.lines
                             =$= CL.decodeUtf8
