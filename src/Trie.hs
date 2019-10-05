{-# Language DerivingStrategies #-}
{-# Language DeriveFunctor #-}
{-# Language TypeFamilies #-}
{-# Language LambdaCase #-}

module Trie where
import Data.Map.Lazy as M hiding (foldl)
import Data.Functor.Foldable
import Data.Maybe (maybe)
import Control.Monad (join)

data Trie k v = Trie (Maybe v) (Map k (Trie k v)) deriving stock Show

data TrieF k v f = TrieF (Maybe v) (Map k f) deriving stock (Show, Functor)

type instance Base (Trie k v) = TrieF k v

instance Recursive (Trie k v) where
  project (Trie u v) = TrieF u v

instance Corecursive (Trie k v) where
  embed (TrieF u v) = Trie u v

size :: Trie k v -> Int
size = cata $ \case
  TrieF p m -> sum m + maybe 0 (const 1) p

lookup :: (Ord k) => Trie k v -> [k] -> Maybe v
lookup = cata . curry $ \case
  (TrieF p _ , []) -> p
  (TrieF _ q, a:as) -> join $ M.lookup a q <*> Just as

empty :: Trie k v
empty = ana (const $ TrieF Nothing M.empty) $ ()

singleton :: [k] -> v -> Trie k v
singleton = curry . ana $ \case
  ([], v) -> TrieF (Just v) M.empty
  y@(p:_,_) -> TrieF Nothing (M.singleton p y)

fromMap :: (Ord k) => Map [k] v -> Trie k v
fromMap = ana coalg where
  coalg mp =
    uncurry TrieF $ foldl x (Nothing, M.empty) (M.toList mp)
  x (_, p) ([], v) = (Just v, p)
  x (m, b) (a:as, v) = (m, M.insertWith M.union a (M.singleton as v) b)

update :: (Ord k) => Trie k v -> [k] -> v  -> Trie k v
update = para palg where
  palg (TrieF _ p) [] a = Trie (Just a) $ fmap fst p
  palg (TrieF x p) (k:ks) a = Trie x $
    case M.lookup k p of
      Nothing -> M.insert k (Trie.singleton ks a) $ fmap fst p
      Just (_, y) -> M.insert k (y ks a) $ fmap fst p

update' :: (Ord k) => Trie k v -> [k] -> v  -> Trie k v
update' = c2 apo
