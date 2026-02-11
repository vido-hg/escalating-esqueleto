{- HLINT ignore "Use camelCase" -}
module EE1a_allFlavors where

import Database.Esqueleto.Experimental
import Schema
import Types

a_allFlavors :: DB [Entity Flavor]
a_allFlavors = do
  select $ do
    from $ table @Flavor

{-
`select` takes a `SqlQuery` argument. `SqlQuery` is a Monad,
so we can use do notation with it to build up our query line by line

So an easily-extensible version of this query is:

a_allFlavors = do
  select $ do
    from $ table @Flavor

But in this case we could simplify to:

a_allFlavors = select $ from $ table @Flavor
-}
