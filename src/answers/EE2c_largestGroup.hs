{- HLINT ignore "Use camelCase" -}
module EE2c_largestGroup where

import Database.Esqueleto.Experimental
import Schema
import Types

-- This answer uses a subquery `SELECT`
-- There are other ways to accomplish the same thing,
-- including using a Common Table Expression (CTE) with SQL's `WITH`
c_largestGroup :: DB [Entity Customer]
c_largestGroup = do
  let
    -- CustomerGroupParent of the largest group
    largestGroupParentId =
      -- this is a subquery, which we extracted into a named variable in Haskell land
      subSelect $ do
        customerLink <- from $ table @CustomerLink
        groupBy customerLink.parentId
        orderBy [desc (countRows :: SqlExpr (Value Int))]
        -- subSelect automatically does a `LIMIT 1` for us to grab at most one item, if at least one row exists
        -- limit 1
        pure customerLink.parentId

  select $ do
    (customer :& customerLink) <- from $
      table @Customer `innerJoin` table @CustomerLink
      `on` (\(customer :& customerLink) -> customer.id ==. customerLink.customerId)

    -- we need 'just' since 'subSelect' returns a 'Value (Maybe a)' (in case no rows are returned by the subquery)
    where_ $ just customerLink.parentId ==. largestGroupParentId

    pure customer
