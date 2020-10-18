{-# LANGUAGE GADTs #-}

module Lexy (Lexer, satisfies, char, string, runLexer) where

import Control.Applicative (Alternative (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Ourlude

data Lexer a where
  Char :: (Char -> Bool) -> Lexer Char
  String :: Text -> Lexer Text
  FMap :: (a -> b) -> Lexer a -> Lexer b
  Pure :: a -> Lexer a
  Ap :: Lexer (a -> b) -> Lexer a -> Lexer b
  Empty :: Lexer a
  Alt :: Lexer a -> Lexer a -> Lexer a
  Star :: Lexer a -> Lexer [a]
  Plus :: Lexer a -> Lexer [a]

instance Show (Lexer a) where
  show (Char _) = "Char <pred>"
  show (String s) = "String " ++ show s
  show (FMap _ a) = "Fmap <func> (" ++ show a ++ ")"
  show (Pure _) = "Pure <value>"
  show (Ap lf la) = "Ap (" ++ show lf ++ ")(" ++ show la ++ ")"
  show Empty = "Empty"
  show (Alt l1 l2) = "Alt (" ++ show l1 ++ ")(" ++ show l2 ++ ")"
  show (Star l) = "Star (" ++ show l ++ ")"
  show (Plus l) = "Plus (" ++ show l ++ ")"

instance Functor Lexer where
  fmap = FMap

instance Applicative Lexer where
  pure = Pure
  (<*>) = Ap

instance Alternative Lexer where
  empty = Empty
  (<|>) = Alt
  many = Star
  some = Plus

satisfies :: (Char -> Bool) -> Lexer Char
satisfies = Char

char :: Char -> Lexer Char
char = (==) >>> Char

string :: Text -> Lexer Text
string = String

runLexer :: Lexer a -> Text -> Maybe (a, Text)
runLexer toRun = case toRun of
  Char ok -> \input ->
    if not (Text.null input) && ok (Text.head input)
      then Just (Text.head input, Text.tail input)
      else Nothing
  String s -> Text.stripPrefix s >>> fmap (\rest -> (s, rest))
  FMap f l -> doFmap f (runLexer l)
  Pure a -> doPure a
  Ap lF lA -> doAp (runLexer lF) (runLexer lA)
  Empty -> const Nothing
  Alt l1 l2 -> doAlt (runLexer l1) (runLexer l2)
  Star l -> doMany (runLexer l)
  Plus l -> doSome (runLexer l)
  where
    doFmap f l = l >>> fmap (\(a, rest) -> (f a, rest))
    doPure a = \input -> Just (a, input)
    doAp lF lA = \input -> do
      (f, rest) <- lF input
      (a, rest') <- lA rest
      return (f a, rest')
    doAlt l1 l2 = \input -> case (l1 input, l2 input) of
      (Nothing, res) -> res
      (res, Nothing) -> res
      (a@(Just (_, restA)), b@(Just (_, restB))) ->
        if Text.compareLength restA (Text.length restB) <= EQ then a else b
    doSome l = doAp (doFmap (:) l) (doMany l)
    doMany l = doAlt (doSome l) (doPure [])
