{- HLINT ignore "Use camelCase" -}
module EE3c_val where

import Database.Esqueleto.Experimental

-- think about why we need a SqlExpr and not just Value here
c_val :: PersistField typ => typ -> SqlExpr (Value typ)
c_val = undefined
