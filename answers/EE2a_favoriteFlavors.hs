{- HLINT ignore "Use camelCase" -}
module EE2a_favoriteFlavors where

import Database.Esqueleto.Experimental
import Schema
import Types

a_favoriteFlavors :: DB [(Entity Customer, Maybe (Entity Flavor))]
a_favoriteFlavors = do
  select $ do
    (customer :& flavor) <- from $
      table @Customer `leftJoin` table @Flavor
      -- Use `leftJoin` because the right side will be a `Maybe`
      -- (`flavor` is `Nothing` when the `on` clause fails)
      --
      -- We always have the left side available--a customer. That customer has a `Maybe (FlavorId)` field.
      -- We can use a `Maybe (FlavorId)` to return a `Maybe (Entity Flavor)`.
      `on` (\(customer :& flavor) -> customer.favoriteFlavor ==. flavor.id)
    pure (customer, flavor)
