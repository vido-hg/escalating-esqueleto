{- HLINT ignore "Use camelCase" -}
module EE1d_dairyFreeFlavors where

import Database.Esqueleto.Experimental
import Schema
import Types

d_dairyFreeFlavors :: DB [Entity Flavor]
d_dairyFreeFlavors = do
  select $ do
    flavor <- from $ table @Flavor
    where_ flavor.dairyFree
    -- You might have instead done something like this, which will work but is more verbose:
    -- where_ $ flavor.dairyFree ==. val True
    pure flavor
