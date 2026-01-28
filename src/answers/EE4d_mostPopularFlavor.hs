{- HLINT ignore "Use camelCase" -}
module EE4d_mostPopularFlavor where

import Data.Coerce (coerce)
import Database.Esqueleto.Experimental
import Schema
import Types

d_mostPopularFlavor :: DB (Maybe FlavorId)
d_mostPopularFlavor = do
  -- need to coerce or unValue
  fmap coerce $ selectOne $ do
    (_customer :& flavor) <- from $
      table @Customer `innerJoin` table @Flavor
      -- need `just` to match up the Maybe-ness through an `innerJoin`
      `on` (\(customer :& flavor) -> customer.favoriteFlavor ==. just flavor.id)
    groupBy flavor.id
    -- need to make the type of `countRows` unambiguous
    orderBy [desc (countRows :: SqlExpr (Value Int))]
    pure flavor.id
