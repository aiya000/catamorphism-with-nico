{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- あるFunctor fに対する、緩いf-代数の表現と、
module Main where

import Data.Char (ord)
import Data.Semigroup ((<>))
import Prelude hiding (length)

-- | あるfに対するf-代数(a, f a -> a)の表現
class Functor f => FAlgebra f a where
  down :: f a -> a -- ^ あるf a -> aの値（なんでもいい）

-- | f-代数aからf-代数bへの準同型写像
data FHomo f a b = FHomo
  { higher :: f a -> f b
  , lower  :: a -> b
  }

-- | 準同型写像はある`Functor f`と`a -> b`から導出できる
fhomo :: Functor f => (a -> b) -> FHomo f a b
fhomo f = FHomo
            { higher = fmap f
            , lower  = f
            }

-- |
-- `FHomo f a b`の満たすべき法則
-- （Haskell上で確認するために、特別にEq制約を追加）
homoLaw :: forall f a b. (FAlgebra f a, FAlgebra f b, Eq b)
            => FHomo f a b  -- ^ 検査の対象
              -> f a -- ^ 始点
              -> Bool
homoLaw FHomo{..} fa =
  let overWay  = lower . down   :: f a -> b
      underWay = down  . higher :: f a -> b
  in overWay fa == underWay fa

-- |
-- f-始代数。
-- あるfについてのf-代数とその準同型写像は圏を為す。
-- `Fix`はちょうどその始対象になる。
-- 始対象になるので、任意の対象(a, f a -> a)に対して射がちょうど1つずつある。
-- （各射がちょうど1つずつあることについてはここで扱わない。扱えない？）
newtype Fix f = Fix
  { unFix :: f (Fix f)
  }

-- | f-始代数から任意のf-代数への射
homoFixToA :: forall f a. FAlgebra f a => FHomo f (Fix f) a
homoFixToA = fhomo f
  where
    f :: Fix f -> a
    f (Fix x) = down $ fmap f x

-- | 実はそのhomoFixToAがちょうどcatamorphismです（ドドーン！！）
cata :: FAlgebra f a => FHomo f (Fix f) a
cata = homoFixToA

-- |
-- `Cons 10 (Cons 20 (Cons 30 Nil)) :: List' Int (List' Int (List' Int b))`
-- のように、型も繰り返すようなリスト型
data List' a b = Nil | Cons a b

instance Functor (List' a) where
   fmap f x = case x of
     Nil      -> Nil
     Cons a b -> Cons a (f b)

-- | `List' a`-代数(Int, List' a Int -> Int)
instance FAlgebra (List' a) Int where
  down :: List' a Int -> Int
  down Nil = 0
  down (Cons _ n) = n + 1

-- | `[] :: [a]`と同じようなもの
nil :: Fix (List' a)
nil = Fix Nil

-- | `(:) :: a -> [a] -> [a]`と同じようなもの
cons :: a -> Fix (List' a) -> Fix (List' a)
cons x xs = Fix $ Cons x xs

-- |
-- catamorphismを用いた、あるaに対する`List' a`向けのlengthの実装。
-- そしてあるaに対する`List' a`-始代数`Fix (List' a)`から
-- `List' a`-代数(Int, List' a Int -> Int)への準同型写像でもある。
-- （例えばaがIntなら`List' Int`-始代数から`List' Int`-代数への準同型写像）
length :: FHomo (List' a) (Fix (List' a)) Int
length = cata

nested' :: List' Int (List' Int (List' Int (List' Int b)))
nested' = Cons 10 (Cons 20 (Cons 30 Nil))

flat' :: Fix (List' Int)
flat' = cons 10 (cons 20 (cons 30 nil)) :: Fix (List' Int)

main :: IO ()
main = putStrLn $ "the length of `flat'` is " <> show  (lower length flat')
