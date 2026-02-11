{- HLINT ignore "Use camelCase" -}
module EE1f_customersWithoutBirthdaysWithFlavors where

import Data.Coerce (coerce)
import Database.Esqueleto.Experimental
import Schema
import Types

f_customersWithoutBirthdaysWithFlavors :: DB [Email]
f_customersWithoutBirthdaysWithFlavors = do
  -- `fmap coerce` gets us from `DB [Value Email]` to `DB [Email]`
  fmap coerce $ select $ do
    customer <- from $ table @Customer
    -- Note that below I used multiple `where_` clauses. This is legal! You can also use the `&&.` (SQL `AND`) operator.
    -- typically, I find multiple `where_` clauses easier to work with than `&&.`
    where_ $ isNothing customer.birthday
    -- be careful about `IS NOT NULL` vs. `!= NULL` when negating isNothing
    -- see the docs: https://hackage.haskell.org/package/esqueleto-3.5.13.0/docs/Database-Esqueleto-Experimental.html#v:isNothing
    where_ $ not_ $ isNothing customer.favoriteFlavor
    pure customer.email
