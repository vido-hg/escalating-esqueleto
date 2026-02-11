{- HLINT ignore "Use camelCase" -}
module EE1c_allFlavorNames where

import Data.Coerce (coerce)
import Data.Text (Text)
import Database.Esqueleto.Experimental
import Schema
import Types

c_allFlavorNames :: DB [Text]
c_allFlavorNames = do
  values <- select $ do
    flavor <- from $ table @Flavor
    pure flavor.name

  -- one way to handle `Value Text -> Text` is with `unValue`:
  -- pure $ map unValue values

  -- note that because `Value` is a newtype, we can often just `coerce` rather than use `unValue`
  -- we could instead do `fmap coerce $ select $ do` above, to avoid binding `values` at all
  pure $ coerce values
