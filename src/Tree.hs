{-# Language DerivingStrategies #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}
{-# Language TypeFamilies #-}
{-# Language LambdaCase #-}
{-# Language TemplateHaskell #-}

module Tree where

import Data.Functor.Foldable.TH
import Data.Functor.Foldable
import Control.Monad (join)

data Tree a = Tree a [Tree a] deriving stock Show

makeBaseFunctor ''Tree

labels :: Tree a -> [a]
labels = cata $ \case
  TreeF a y -> a : join y

data P a = Z a | S (P (a,a)) deriving stock Show
makeBaseFunctor ''P
