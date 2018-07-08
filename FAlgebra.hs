{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | あるfに対するf-代数と、そのIntList-代数の表現
module Main where

import Data.Semigroup ((<>))

-- | あるfに対するf-代数(a, f a -> a)の表現
class Functor f => FAlgebra f a where
  down :: f a -> a -- ^ あるf a -> aの値

-- |
-- `Cons 10 (Cons 20 (Cons 30 Nil)) :: List Int (List Int (List Int (List Int b)))`
-- のように型が再帰するリスト
data List a b = Nil | Cons a b
  deriving (Show)

-- | わかりやすさのためにIntに特殊化する
type IntList = List Int

-- | IntListがf-代数のfになれるようにする
instance Functor IntList where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons x $ f xs

-- | IntList-代数 (String, IntList String -> String)
instance FAlgebra IntList String where
  down :: IntList String -> String
  down Nil = []
  down (Cons _ xs) = '0' : xs

-- | IntList-代数 (Int, IntList Int -> Int)
instance FAlgebra IntList Int where
  down :: IntList Int -> Int
  down Nil = 0
  down (Cons _ n) = n + 1

checkList :: IO ()
checkList = do
  let xs = Cons 10 (Cons 20 (Cons 30 Nil)) :: IntList (IntList (IntList (IntList ())))
  print xs
-- {output}
-- Cons 10 (Cons 20 (Cons 30 Nil))

-- | f-代数aからf-代数bへの準同型写像
data FHomo f a b = FHomo
  { higher :: f a -> f b
  , lower  :: a -> b
  }

-- |
-- スマートコンストラクタ。
-- 準同型写像はある`Functor f`と`a -> b`から導出できる。
fhomo :: Functor f => (a -> b) -> FHomo f a b
fhomo f = FHomo
            { higher = fmap f
            , lower  = f
            }

-- | IntList-代数StringからIntの準同型写像
homoStringToInt :: FHomo IntList String Int
homoStringToInt = fhomo length

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

checkFHomo :: IO ()
checkFHomo = do
  let answer = homoLaw homoStringToInt Nil && homoLaw homoStringToInt (Cons 10 "xyz")
  putStrLn $ "Is homoStringToInt a valid homomorphism?: " <> show answer
-- {output}
-- Is homoStringToInt a valid homomorphism?: True

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
-- catamorphismを用いた、あるaに対するIntList向けのlengthの実装。
-- そしてIntList-始代数`Fix IntList`から
-- IntList-代数(Int, IntList Int -> Int)への準同型写像でもある。
length' :: FHomo IntList (Fix IntList) Int
length' = cata

-- | Fix版の値構築子Nil
nil :: Fix IntList
nil = Fix Nil

-- | Fix版の値構築子Cons
cons :: Int -> Fix IntList -> Fix IntList
cons x xs = Fix $ Cons x xs

flat :: Fix IntList
flat = cons 10 . cons 20 $ cons 30 nil

nested :: IntList (IntList (IntList (IntList b)))
nested = Cons 10 (Cons 20 (Cons 30 Nil))

main :: IO ()
main = do
  checkList
  checkFHomo
  putStrLn $ "the length is " <> show (lower length' flat)
