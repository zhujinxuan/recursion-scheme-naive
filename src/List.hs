{-# Language LambdaCase #-}
{-# Language ViewPatterns #-}

module List
  where
import Numeric.Natural (Natural)

import Data.Functor.Foldable

-- data ListF a b = Nil | Cons a b

-- Cons a Bool
has :: (a -> Bool) -> [a] -> Bool
has f = cata $ \case
  Nil -> False
  Cons a b -> f a || b

-- Cons a Bool
filter :: (a -> Bool) -> [a] -> [a]
filter f = cata $ \case
  Nil -> []
  Cons (f -> False) b -> b
  Cons a b -> a : b

-- Cons a Int
length :: [a] -> Int
length = cata $ \case
  Nil -> 0
  Cons _ b -> 1 + b

-- Cons a (Maybe a)
max :: (Ord a) => [a] -> Maybe a
max = cata $ \case
  Nil -> Nothing
  Cons p Nothing -> Just p
  Cons q (Just x) -> Just $ if q > x then q else x

-- Cons a (Natural -> Maybe a)
lookup :: [a] -> Natural  -> Maybe a
lookup = cata . curry $ \case
  (Nil, _) -> Nothing
  (Cons p _, 0) -> Just p
  (Cons _ q, y) -> q (y - 1)

many :: Natural -> a ->  [a]
many = curry . ana $ \case
  (0, a) -> Nil
  (x, a) -> Cons a (x - 1, a)

-- delete1 :: [a] -> Int -> [a]
-- delete1 = cata . curry $ \case
--   (Nil, _) -> []
--   (Cons a q, 0) -> q (-1)
--   (Cons a q, y) -> a : q (y-1)

delete :: [a] -> Natural -> [a]
delete = para dPalg where
  dPalg Nil _ = []
  dPalg (Cons a (u, f)) 0 = u
  dPalg (Cons a (u, f)) p = a : f (p - 1)

delete' :: [a] -> Natural -> [a]
delete' = curry . apo $ \case
  ([], _) -> Nil
  (_:as, 0) -> fmap Left $ project as
  (a:as, p) -> Cons a (Right (as, p - 1))

update :: [a] -> Natural -> a -> [a]
update = para palg where
  palg Nil _ _ = []
  palg (Cons a (u, _)) 0 b = b : u
  palg (Cons a (u, f)) n b = a : f (n-1) b

update' :: [a] -> Natural -> a -> [a]
update' = c2 (apo acoalg) where
  c2 f a b c = f (a,b,c)
  acoalg ([], _, _) = Nil
  acoalg (_:as , 0, b) = Cons b $ Left as
  acoalg (a:as , n, b) = Cons a $ Right (as, n-1, b)
