{- HLINT ignore "Use camelCase" -}
module EE1b_allFlavorNameValues where

import Data.Text (Text)
import Database.Esqueleto.Experimental
import Schema
import Types

-- Note again that this generates SQL to select a single column
-- and doesn't filter "after the fact" in Haskell
b_allFlavorNameValues :: DB [Value Text]
b_allFlavorNameValues = do
  select $ do
    flavor <- from $ table @Flavor -- get `Flavor` `Entity`s from the table
    pure flavor.name -- project the name field specifically
