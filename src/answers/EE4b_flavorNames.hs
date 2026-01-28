{- HLINT ignore "Use camelCase" -}
module EE4b_flavorNames where

import Data.Coerce (coerce)
import Data.Text (Text)
import Database.Esqueleto.Experimental
import Schema
import Types

b_flavorNames :: DB [Text]
b_flavorNames = do
  fmap coerce $ select $ do
    flavor <- from $ table @Flavor
    pure flavor.name
