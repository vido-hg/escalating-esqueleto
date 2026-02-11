{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE TypeOperators #-}
module EE3f_leftJoin where

import Database.Esqueleto.Experimental
import Database.Esqueleto.Experimental.From (ToFrom)
import Database.Esqueleto.Experimental.From.Join (HasOnClause)

f_leftJoin :: (ToFrom a a', ToFrom b b', ToMaybe b', HasOnClause rhs (a' :& ToMaybeT b'), rhs ~ (b, (a' :& ToMaybeT b') -> SqlExpr (Value Bool))) => a -> rhs -> From (a' :& ToMaybeT b')
f_leftJoin = undefined
