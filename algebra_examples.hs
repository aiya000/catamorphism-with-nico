-- | f-代数のfの例
module AlgebraExamples where

import Data.Functor.Identity (Identity(..))
import FAlgebra hiding (main)

{-# ANN module "HLint: ignore Unnecessary hiding" #-}

-- | Fix版の値構築子Identity（これは定義できる）
identity :: Fix Identity -> Fix Identity
identity = Fix . Identity

-- ↑の値構築子の型
-- Fix      :: Identity (Fix Identity) -> Fix Identity
-- Identity :: Fix Identity -> Identity (Fix Identity)

{-

-- | Identityは基底部がないので値が定義できない！
x :: Fix Identity
x = identity (identity (identity (...??)))

-}

-- | Fix版の値構築子Nothing
nothing :: Fix Maybe
nothing = Fix Nothing

-- ↑の値構築子の型
-- Fix     :: Maybe (Fix Maybe) -> Fix Maybe
-- Nothing :: Maybe (Fix Maybe)

-- | Fix版の値構築子Just
just :: Fix Maybe -> Fix Maybe
just = Fix . Just

-- ↑の値構築子の型
-- Fix  :: Maybe (Fix Maybe) -> Fix Maybe
-- Just :: Fix Maybe -> Maybe (Fix Maybe)

-- | 再帰の基底部`Nothing :: forall a. Maybe a`のおかげで、値が定義できた！
x :: Fix Maybe
x = just (just (just nothing))

y :: Fix Maybe
y = just (just nothing)

z :: Fix Maybe
z = just nothing

main :: IO ()
main = pure ()
