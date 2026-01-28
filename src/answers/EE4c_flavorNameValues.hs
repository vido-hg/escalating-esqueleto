{- HLINT ignore "Use camelCase" -}
module EE4c_flavorNameValues where

import Data.Text (Text)
import Database.Esqueleto.Experimental
import Schema
import Types

c_flavorNameValues :: DB [Value Text]
c_flavorNameValues = do
  select $ do
    flavor <- from $ table @Flavor
    pure flavor.name
