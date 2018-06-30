{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- あるFunctor fに対する、緩いf-代数の表現と、
-- MaybeをfとしたMaybe-代数について。
module Main where

import Control.Arrow ((>>>))
import Data.Char (ord)
import Data.Semigroup ((<>))

-- |
-- あるfに対するf-代数(a, f a -> a)。
-- 表現の都合上a -> fの依存性があるが、本質には関係ない。
class Functor f => FAlgebra f a | a -> f where
  raw  :: a        -- ^ あるaの値（なんでもいい）
  down :: f a -> a -- ^ あるf a -> aの値（なんでもいい）

-- | (Maybe Char -> Char, Char)のMaybe-代数
instance FAlgebra Maybe Int where
  raw = 10
  down (Just x) = x
  down Nothing  = 20

-- | (Maybe Char -> Char, Char)のMaybe-代数
instance FAlgebra Maybe Char where
  raw = 'a'
  down (Just x) = x
  down Nothing  = 'x'

-- | f-代数aからf-代数bへの準同型写像
data FHomo f a b = FHomo
  { higher :: f a -> f b
  , lower  :: a -> b
  }

-- | 準同型写像はある`Functor f`と`a -> b`から定義できる
fhomo :: Functor f => (a -> b) -> FHomo f a b
fhomo f = FHomo
            { higher = fmap f
            , lower  = f
            }

-- | Maybe-代数Char から Maybe-代数Int への準同型
homoCharToInt :: FHomo Maybe Char Int
homoCharToInt = fhomo f
  where
    f :: Char -> Int
    f = ord

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

main :: IO ()
main = putStrLn $ "is homoCharToInt a homomorphism?: " <> show isHomoCharIntHomo
  where
    isHomoCharIntHomo :: Bool
    isHomoCharIntHomo = homoLaw homoCharToInt (Just 'a')
