{-# LANGUAGE LambdaCase #-}

module Stutter.Parser where

import Control.Applicative
import Control.Monad
import Text.Read (readMaybe)
import Data.Attoparsec.Text ((<?>))

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as T

import Stutter.Producer hiding (ProducerGroup)

type ProducerGroup = ProducerGroup_ ()

-------------------------------------------------------------------------------
-- Text
-------------------------------------------------------------------------------

parseText :: Atto.Parser T.Text
parseText = (<?> "text") $
    T.pack <$> Atto.many1 parseSimpleChar

parseSimpleChar :: Atto.Parser Char
parseSimpleChar = (<?> "simple char or escaped char") $
    -- A non-special char
    Atto.satisfy (`notElem` specialChars) <|>
    -- An escaped special char
    Atto.char '\\' *> Atto.anyChar

specialChars :: [Char]
specialChars =
    [
    -- Used for sum
      '+'
    -- Used for product
    , '*'
    -- Used to delimit ranges
    , '[', ']'
    -- Used to scope groups
    , '(', ')'
    -- Used for escaping
    , '\\'
    -- Used for files
    , '@'
    ]

parseGroup :: Atto.Parser ProducerGroup
parseGroup = (<?> "producer group") $
    (PSum <$> parseUnit <* Atto.char '+' <*> parseGroup)     <|>
    (PProduct <$> parseUnit <* Atto.char '*' <*> parseGroup) <|>
    -- Parse product with implicit '*'
    (PProduct <$> parseUnit <*> parseGroup)                  <|>
    parseUnit

parseUnit :: Atto.Parser ProducerGroup
parseUnit = (<?> "unary producer") $
    PRanges <$> parseRanges <|>
    parseHandle             <|>
    PText <$> parseText     <|>
    bracketed parseGroup
  where
    bracketed :: Atto.Parser a -> Atto.Parser a
    bracketed p = Atto.char '(' *> p <* Atto.char ')'


-- | Parse a Handle-like reference, preceded by an @\@@ sign. A single dash
-- (@-@) is interpreted as @stdin@, any other string is used as a file path.
parseHandle :: Atto.Parser ProducerGroup
parseHandle = (<?> "handle reference") $
    (flip fmap) parseFile $ \case
      "-" -> PStdin ()
      fp -> PFile fp

-------------------------------------------------------------------------------
-- File
-------------------------------------------------------------------------------

parseFile :: Atto.Parser FilePath
parseFile = (<?> "file reference") $
    T.unpack <$> (Atto.char '@' *> parseText)

-------------------------------------------------------------------------------
-- Ranges
-------------------------------------------------------------------------------

-- | Parse several ranges
--
-- Example:
--  @[a-zA-Z0-6]@
parseRanges :: Atto.Parser [Range]
parseRanges = (<?> "ranges") $
    Atto.char '['          *>
    Atto.many1 parseRange <*
    Atto.char ']'

-- | Parse a range of the form 'a-z' (int or char)
parseRange :: Atto.Parser Range
parseRange = (<?> "range") $
    parseIntRange <|> parseCharRange

-- | Parse a range in the format "<start>-<end>", consuming exactly 3
-- characters
parseIntRange :: Atto.Parser Range
parseIntRange = (<?> "int range") $
    IntRange <$>  ((,) <$> parseInt <* Atto.char '-' <*> parseInt)
  where
    parseInt :: Atto.Parser Int
    parseInt = (readMaybe . (:[]) <$> Atto.anyChar) >>= \case
      Nothing -> mzero
      Just x -> return x

-- | Parse a range in the format "<start>-<end>", consuming exactly 3
-- characters
parseCharRange :: Atto.Parser Range
parseCharRange = (<?> "char range") $
    CharRange <$> ((,) <$> Atto.anyChar <* Atto.char '-' <*> Atto.anyChar)
