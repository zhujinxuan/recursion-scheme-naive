{-# Language TypeApplications #-}

module Parser where

import Control.Monad (join)

import Data.Functor.Foldable
rp :: (Monad m, Corecursive t) => ((Base t (m t)) -> m (Base t t)) -> (m x -> m (Base t (m x))) -> m x -> m t
rp c y = f where
  f = fmap embed . join . fmap c . (fmap (fmap f)) . y



