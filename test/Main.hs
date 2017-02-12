{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty (TestTree, defaultMainWithIngredients, defaultIngredients)
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.Runners.AntXML (antXMLRunner)

import qualified Data.Attoparsec.Text as Atto
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty

import Stutter.Producer (Range(..), ProducerGroup_ (..))

import qualified Stutter.Parser as Stutter

main :: IO ()
main =
    defaultMainWithIngredients (antXMLRunner:defaultIngredients) $ parserTests

parserTests :: TestTree
parserTests =
  Tasty.testGroup "parser"
    [ Tasty.testCase "parses a char range" $
        (Right $ CharRange ('a', 'z'))            @=?
        (Atto.parseOnly Stutter.parseRange "a-z")
    , Tasty.testCase "parses an int range" $
        (Right $ IntRange (0, 3))                 @=?
        (Atto.parseOnly Stutter.parseRange "0-3")
    , Tasty.testCase "parses 1-range group" $
        (Right [IntRange (0, 3)])                    @=?
        (Atto.parseOnly Stutter.parseRanges "[0-3]")
    , Tasty.testCase "parses 2-range group" $
        (Right [IntRange (0, 3), CharRange ('A', 'Z')]) @=?
        (Atto.parseOnly Stutter.parseRanges "[0-3A-Z]")
    , Tasty.testCase "parses a text" $
        (Right "abcd") @=? (Atto.parseOnly Stutter.parseText "abcd")
    , Tasty.testGroup "parses a text with escaped chars "
        [ Tasty.testCase "escaped brackets" $
            (Right "abcd[") @=? (Atto.parseOnly Stutter.parseText "abcd\\[")
        , Tasty.testCase "escaped backslash" $
            (Right "abcd\\") @=? (Atto.parseOnly Stutter.parseText "abcd\\\\")
        ]
    , Tasty.testCase "parses a sum" $
        (Right $ PSum (PText "foo") (PText "bar"))    @=?
        (Atto.parseOnly Stutter.parseGroup "foo+bar")
    , Tasty.testCase "parses a multi-sum" $
        (Right
          (PSum (PText "foo")
                (PSum (PText "bar")
                      (PText "baz")))) @=?
        (Atto.parseOnly Stutter.parseGroup "foo+(bar+baz)")
    , Tasty.testCase "parses a multi-sum bis" $
        (Right
          (PSum (PSum (PText "foo")
                      (PText "bar"))
                (PText "baz")))                              @=?
        (Atto.parseOnly Stutter.parseGroup "(foo+bar)+baz")
    , Tasty.testCase "parses a multi-sum (right associative)" $
        (Right
          (PSum (PText "foo")
                (PSum (PText "bar")
                      (PText "baz")))) @=?
        (Atto.parseOnly Stutter.parseGroup "foo+bar+baz")
    , Tasty.testCase "parses a multi-product (right associative)" $
        (Right
          (PProduct (PText "foo")
                    (PProduct (PText "bar")
                              (PText "baz")))) @=?
        (Atto.parseOnly Stutter.parseGroup "foo*bar*baz")
    , Tasty.testCase "parses an implicit product" $
        (Right
          (PProduct (PText "foo")
                    (PText "bar"))) @=?
        (Atto.parseOnly Stutter.parseGroup "(foo)(bar)")
    , Tasty.testCase "parses file" $
        (Right (PFile "test.txt"))                    @=?
        (Atto.parseOnly Stutter.parseGroup "@test.txt")
    , Tasty.testCase "parses stdin ref" $
        (Right $ PStdin ())                 @=?
        (Atto.parseOnly Stutter.parseGroup "@-")
    ]
