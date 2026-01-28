{- HLINT ignore "Use camelCase" -}
module EE4a_getChocolate where

import Database.Esqueleto.Experimental
import Schema
import Types

a_getChocolate :: DB (Maybe (Entity Flavor))
a_getChocolate = do
  selectOne $ do
    flavor <- from $ table @Flavor
    -- (==.) and missing `val`
    where_ $ flavor.name ==. val "Chunky Chocolate"
    pure flavor
