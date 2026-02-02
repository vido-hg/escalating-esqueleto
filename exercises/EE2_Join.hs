{- HLINT ignore "Use camelCase" -}
module EE2_Join where

import Data.Coerce (coerce)
import Data.Text (Text)
import Database.Esqueleto.Experimental
import Database.Esqueleto.PostgreSQL
import Data.Text (Text)
import Schema
import Types

{-
What are all our customers' favorite flavors?

If they don't have one, give back a `Nothing`.

SELECT c.name, f.name
FROM Customer c
LEFT JOIN Flavor f ON c.favoriteFlavor = f.Id

-}
a_favoriteFlavors :: DB [(Entity Customer, Maybe (Entity Flavor))]
-- a_favoriteFlavors = _
a_favoriteFlavors = do
  select $ do
    (c :& f) <- from $ 
      table @Customer `leftJoin` table @Flavor 
      `on` (\(c :& f) -> c.favoriteFlavor ==. f.id)
    pure (c, f)

{-
We'd like to determine the popularity of each flavor.

Return a list of each flavor name along with how many customers have it as
their favorite flavor. Sort it by popularity in descending order,
and then alphabetically by name.

Sample results:
[ ("Chunky Chocolate", 27)
, ("Smooth Strawberry", 12)
, ("Coconut Cream", 3)
, ("Variegated Vanilla", 3)
]

SELECT f.name, COUNT(c.id)
FROM Flavor f
JOIN Customer c ON f.id = c.favoriteFlavor
GROUP BY f.name
ORDER BY COUNT(c.id) DESC, f.name ASC;

-}
b_flavorPopularity :: DB [(Text, Int)]
-- b_flavorPopularity = _
b_flavorPopularity = do
  rows <- select $ do
    (f :& c) <- from $ 
      table @Flavor `innerJoin` table @Customer 
      `on` (\(f :& c) -> just (f.id) ==. c.favoriteFlavor)
    let cnt = count (c.id)
    groupBy f.name
    orderBy [desc cnt, asc f.name]
    pure (f.name, cnt)
  pure $ coerce (rows :: [(Value Text, Value Int)])

{-
We have a concept of "groups" provided by CustomerLink and CustomerGroupParent.

Who are all the customers in the largest group?

SELECT c.name
FROM Customer c
JOIN CustomerLink l ON c.id = l.customerId
WHERE l.parentId = (
    SELECT parentId
    FROM CustomerGroupParent
    GROUP BY parentId
    ORDER BY COUNT(parentId) DESC
    LIMIT 1
)

-}
c_largestGroup :: DB [Entity Customer]
-- c_largestGroup = _
c_largestGroup = do
  let largestGroupParentId = 
        subSelect $ do
          g <- from $ table @CustomerLink
          groupBy g.parentId
          orderBy [desc (countRows :: SqlExpr (Value Int))]
          limit 1
          pure g.parentId
  select $ do
    (c :& l) <- from $ table @Customer `innerJoin` table @CustomerLink 
      `on` (\(c :& l) -> c.id ==. l.customerId)
    where_ $ just (l.parentId) ==. largestGroupParentId
    pure c

{-
For each CustomerGroupParent with at least one customer in it, list its name,
as well as the names of all the customers in that group.

SELECT g.name, ARRAY_AGG(c.name) AS customers
FROM CustomerGroupParent g
INNER JOIN CustomerLink l ON g.id = l.parentId
INNER JOIN Customer c ON l.customerId = c.id
GROUP BY g.name

-}
d_customerGroups :: DB [(Text, [Text])]
-- d_customerGroups  = _
d_customerGroups = do
  rows <- select $ do
    (g :& l :& c) <- from $ 
      table @CustomerGroupParent 
        `innerJoin` table @CustomerLink 
            `on` (\(g :& l) -> g.id ==. l.parentId) 
        `innerJoin` table @Customer 
            `on` (\((g :& l) :& c) -> l.customerId ==. c.id)
    groupBy g.name
    -- orderBy [asc g.name]
    pure (g.name, maybeArray $ arrayAgg c.name)
  pure $ coerce (rows :: [(Value Text, Value [Text])])