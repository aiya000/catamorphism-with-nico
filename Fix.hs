module Main where

-- | Fix f = f (Fix f)
newtype Fix f = Fix
  { unFix :: f (Fix f)
  }

{- 
Identityは基底部がないので値が定義できない！

Fix Identity
= Identity (Fix Identity)
= Identity (Identity (Fix Identity))
= Identity (Identity (Identity (Fix (...))))

x :: Fix Identity
x = Fix (Identity (Fix (Identity (Fix ...?)))
-}

-- | Nothing :: Maybe (Fix b)
x :: Fix Maybe
x = Fix (Just (Fix Nothing))

main :: IO ()
main = pure ()
