{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Arrow ((>>>))
import Data.Char (ord)
import Data.Semigroup ((<>))

-- | F-代数の定義
data FAlgebra f a = FAlgebra
  { object :: a
  , morph  :: f a -> a
  }

-- | (Maybe Char, Char)のF-代数
fMaybeChar :: FAlgebra Maybe Char
fMaybeChar = FAlgebra
              { object = 'a'
              , morph  = f
              }
  where
    f = \case
      Nothing -> '0'
      Just a  -> a

-- | (Maybe Int -> Int, Int)のF-代数
fMaybeInt :: FAlgebra Maybe Int
fMaybeInt = FAlgebra
             { object = 10
             , morph  = f
             }
  where
    f = \case
      Nothing -> 0
      Just a  -> a

-- |
-- あるFunctor f, a, bに対して
-- FAlgebra f aからFAlgebra f bへの準同型写像
data FHomo f a b = FHomo
  { higher :: f a -> f b
  , lower  :: a -> b
  }

-- |
-- スマートコンストラクタ。
-- 準同型写像はある`Functor f`と`a -> b`から定義される
homo :: Functor f => (a -> b) -> FHomo f a b
homo f = FHomo
          { higher = fmap f
          , lower  = f
          }

-- |
-- fmap fと合わせて
-- FAlgebra Maybe Char から FAlgebra Maybe Int への準同型
homoCharInt :: FHomo Maybe Char Int
homoCharInt = homo f
  where
    f :: Char -> Int
    f = ord

-- |
-- `FHomo f a b`の満たすべき法則
-- （Haskell上で確認するために、特別にEq制約を追加）
homoLaw :: forall f a b. (Functor f, Eq b)
            => FHomo f a b  -- ^ 検査の対象
            -> f a          -- ^ 始点
            -> FAlgebra f a -- ^ `f a -> a`
            -> FAlgebra f b -- ^ `f b -> b`
            -> Bool
homoLaw FHomo{..} fa FAlgebra{morph = downToA} FAlgebra{morph = downToB} =
  let overWay  = lower . downToA  :: f a -> b
      underWay = downToB . higher :: f a -> b
  in overWay fa == underWay fa

main :: IO ()
main = putStrLn $ "is homoCharInt a homomorphism?: " <> show isHomoCharIntHomo
  where
    isHomoCharIntHomo :: Bool
    isHomoCharIntHomo = homoLaw homoCharInt (Just 'a') fMaybeChar fMaybeInt
