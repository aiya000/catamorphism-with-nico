{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 一般におけるcataとfoldrの等価性を示す変相互換
module CataFoldrEquality where

import FAlgebra hiding (cata, main, cons, nil, xs)
import Prelude hiding (foldr)

{-# ANN module "HLint: ignore Unnecessary hiding" #-}

-- | `Functor IntList`インスタンスを一般化
instance Functor (List a) where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons x $ f xs

-- | `nil :: Fix IntList`を一般化
nil :: Fix (List a)
nil = Fix Nil

-- | `cons :: Int -> Fix IntList -> Fix IntList`を一般化
cons :: a -> Fix (List a) -> Fix (List a)
cons x xs = Fix $ Cons x xs

-- |
-- `cata :: FAlgebra f a => FHomo f (Fix f) a`を一般化。
-- `f a -> a`がFAlgebraから解放され、冗長なFHomoの表現はもはや必要とされない。
cata :: Functor f => (f a -> a) -> Fix f -> a
cata f = f . fmap (cata f) . unFix

-- |
-- Fix (List a)と[a]が等価な表現であること
-- （双対は右辺と左辺を反転すると作れる）
unusual :: [a] -> Fix (List a)
unusual [] = Fix Nil
unusual (x:xs) = cons x $ unusual xs

{-# ANN unusual "HLint: ignore Use foldr" #-}

-- |
-- `Cons x xs`のxとxsを渡された関数で合わせるか、
-- もしくは`Nil`をそのまま返す。
plus :: (a -> b -> b) -> b -> List a b -> b
plus _ def Nil = def
plus f _ (Cons x z) = f x z

-- | 各変換からcataで構成されたfoldr
foldr :: forall a b. (a -> b -> b) -> b -> [a] -> b
foldr f init = cata (plus f init) . unusual

-- リスト型に特殊化した、一般的なfoldr
-- foldr :: (a -> b -> b) -> b -> [a] -> b

main :: IO ()
main = print $ foldr (+) 0 [1, 2, 3]
