module Main where

import Data.Functor.Identity (Identity(..))

-- | Fix f = f (Fix f)
newtype Fix f = Fix
  { unFix :: f (Fix f)
  }

-- | Fix版の値構築子Identity（これは定義できる）
identity :: Fix Identity -> Fix Identity
identity x =
  (Fix :: Identity (Fix Identity) -> Fix Identity) $
    (Identity :: Fix Identity -> Identity (Fix Identity)) x

-- もとい
-- identity = Fix . Identity

-- | Identityは基底部がないので値が定義できない！
-- x :: Fix Identity
-- x = identity (identity (identity (...?)))

-- 脱identity
-- x = Fix (Identity (Fix (Identity (Fix (Identity (...?))))))

-- | Fix版の値構築子Nothing
nothing :: Fix Maybe
nothing =
  (Fix :: Maybe (Fix Maybe) -> Fix Maybe)
    (Nothing :: Maybe (Fix Maybe))

-- もとい
-- nothing = Fix Nothing

-- | Fix版の値構築子Just
just :: Fix Maybe -> Fix Maybe
just x =
  (Fix :: Maybe (Fix Maybe) -> Fix Maybe) $
    (Just :: Fix Maybe -> Maybe (Fix Maybe)) x

-- もとい
-- just = Fix . Just

-- |
-- Fix版（フラット版）の`Just (Just (Just Nothing)) :: Maybe (Maybe (Maybe (Maybe a)))`。
-- 再帰の基底部`Nothing :: forall a. Maybe a`のおかげで、値が定義できた！
x :: Fix Maybe
x = just (just (just nothing))

-- 脱nothing, 脱just
-- x = Fix (Just (Fix Nothing))

y :: Fix Maybe
y = just (just nothing)

z :: Fix Maybe
z = just nothing

main :: IO ()
main = pure ()
