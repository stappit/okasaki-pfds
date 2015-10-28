module Chap03.Data.TaggedBinaryTree where

import Data.Foldable
import Prelude hiding (foldr)

data TaggedBinaryTree m a = E
                          | T m a (TaggedBinaryTree m a) (TaggedBinaryTree m a)
                          deriving (Eq)

instance (Show m, Show a) => Show (TaggedBinaryTree m a) where
  show E = "E"
  show (T m x a b) = "(" ++ unwords [show m, show x, show a, show b] ++ ")"

tag :: m -> TaggedBinaryTree m a -> m
tag m E           = m
tag _ (T n _ _ _) = n

instance Foldable (TaggedBinaryTree m) where
  foldr _ z E           = z
  foldr f z (T _ x a b) = foldr f (f x $ foldr f z b) a

instance Functor (TaggedBinaryTree m) where
  fmap _ E = E
  fmap f (T m x a b) = T m (f x) (fmap f a) (fmap f b)
