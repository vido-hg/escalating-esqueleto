{- HLINT ignore "Use camelCase" -}
module EE2b_flavorPopularity where

import Data.Coerce (coerce)
import Data.Text (Text)
import Database.Esqueleto.Experimental
import Schema
import Types

b_flavorPopularity :: DB [(Text, Int)]
b_flavorPopularity = do
  values <- select $ do

    -- This `_customer` and `flavor` are not the same as the ones in the `on` clause.
    -- Because `_customer` is unused (in the `groupBy` etc.), we prefix with an underscore
    (_customer :& flavor) <- from $

      -- Use `innerJoin` because our tally will only count up `customer.favoriteFlavor` fields that aren't null
      table @Customer `innerJoin` table @Flavor

      -- `customer.favoriteFlavor` is a `Maybe`, so we add `just` on the
      -- right hand side of the `==.` to make both sides a `Maybe`
      --
      -- Note that this interacts with the kind of join you use.
      -- Here, an `innerJoin` means that both `customer` and `flavor` are not `Maybe`
      -- But the _fields_ on them can still be `Maybe`s!
      `on` (\(customer :& flavor) -> customer.favoriteFlavor ==. just flavor.id)
      -- ^ `customer` and `flavor` are scoped to just this line

    groupBy flavor.id

    -- `countRows` is the equivalent of `COUNT(*)`. It has type:
    -- countRows :: Num a => SqlExpr (Value a)
    -- so we specify that `Num` to be an `Int` here
    orderBy [desc (countRows :: SqlExpr (Value Int)), asc flavor.name]

    pure (flavor.name, countRows)

  -- We could use `unValue` to go from `(Value Text, Value Int)` to `(Text, Int)`:
  -- pure $ map (\(name, count') -> (unValue name, unValue count')) values

  -- because `Value` is a newtype, you can instead just coerce. `countRows` is polymorphic though, over any `Num a`.
  -- we give `values` a concrete type here to give the typechecker enough information to coerce
  pure $ coerce (values :: [(Value Text, Value Int)])
  -- another good option is to include this type annotation in a `values :: [(Value Text, Value Int)] <- select $ do` line above.
