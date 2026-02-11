{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types where

import Control.Monad.Logger (runStdoutLoggingT, LoggingT)
import Data.Aeson qualified as Aeson
import Data.Fixed (Centi)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.UUID (UUID, fromASCIIBytes, toASCIIBytes, toText, fromText)
import Database.Esqueleto.Experimental
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn)
import DerivePostgresEnumTH (derivePostgresEnum)
import Language.Haskell.TH (Q, Exp)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Lift(..))
import Text.Regex.PCRE.Heavy (Regex, re, (=~))
import Web.PathPieces (PathPiece(..))


-- DB MONAD --


-- This type for DB is not very safe. It allows nested 'runDB's and lifting of arbitrary IO actions.
-- It's also not thread-safe, ideally it would use a Pool of connections rather than one.
-- However, it's good enough for these exercises, if a little simplified.
--
-- We start with a Reader of a SqlBackend, over IO
-- type DB = ReaderT SqlBackend IO
--
-- There's an existing synonym for a Reader of a SqlBackend
-- type SqlPersistT = ReaderT SqlBackend
--
-- We also include logging as required by 'withPostgresqlConn', and to print rendered SQL
type DB = SqlPersistT (LoggingT IO)

-- Run a DB action (e.g., an esqueleto `select` query)
-- This uses the connection string to connect to the database, runs your
-- query on that database, and logs to standard out along the way.
runDB :: DB a -> IO a
runDB action = runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
  runSqlConn action backend

-- Basic information needed to connect to a local postgres instance
connectionString :: ConnectionString
connectionString = "host=127.0.0.1 port=5432 user=escalatingesqueleto dbname=escalatingesqueleto"


-- CUSTOM TYPES --


-- Helper to convert Show-ables to Text
tshow :: Show a => a -> Text
tshow = T.pack . show


-- UUID
-- 'PersistField' lets us convert a type (in this case a 'UUID') to and from a 'PersistValue'
-- 'PersistValue's can be stored in the database
instance PersistField UUID where
  -- A 'UUID' is a literal, escaped, collction of ASCII bytes
  toPersistValue = PersistLiteral_ Escaped . toASCIIBytes

  -- If we see a 'PersistLiteral_ Escaped', there's a chance it's a 'UUID'. Keep going.
  fromPersistValue (PersistLiteral_ Escaped uuid) =
    case fromASCIIBytes uuid of
      -- Errors if the UUID format is invalid
      Nothing -> Left $ "Types.hs: Failed to deserialize a UUID. Instead received: " <> tshow uuid
      -- Parses out a UUID if the format is valid
      Just uuid' -> Right uuid'
  -- Any other kind of 'PersistValue' is unexpected
  fromPersistValue x = Left $ "Types.hs: When trying to deserialize a UUID: expected PersistDbSpecific, received: " <> tshow x

-- 'PersistFieldSql' tells the database what database-level type to use for a Haskell type
-- Postgres supplies a 'uuid' type, so we can use that for a Haskell 'UUID'
instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

instance PathPiece UUID where
  toPathPiece = toText
  fromPathPiece = fromText


-- Email. With a regex for usage with the smart constructor below
newtype Email = Email Text
  deriving stock (Show, Lift)
  deriving newtype (Eq, Ord)

emailRegex :: Regex
emailRegex = [re|^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$|]

emailToText :: Email -> Text
emailToText (Email t) = t

-- | Attempt to create an 'Email' from 'Text', returning 'Nothing' if it doesn't
-- match our email regex
mkEmail :: Text -> Maybe Email
mkEmail t = if t =~ emailRegex
  then Just (Email t)
  else Nothing

instance PersistField Email where
  toPersistValue email = PersistLiteral_ Escaped $ TE.encodeUtf8 $ emailToText email
  fromPersistValue (PersistLiteral_ Escaped bs) = case mkEmail $ TE.decodeUtf8 bs of
    Nothing -> Left . T.pack $ "lib/Types.hs: Deserialized invalid email from the database, which should never happen " <> show bs
    Just email -> Right email
  fromPersistValue x = Left . T.pack $ "lib/Types.hs: When trying to deserialize an `Email`, expected PersistDbSpecific, received: " <> show x

instance PersistFieldSql Email where
  sqlType _ = SqlOther "email"

-- | Create an email at compile time
-- Usage: [compileEmail|mitchell@mercury.com|]
compileEmail :: QuasiQuoter
compileEmail =
  QuasiQuoter
    { quoteExp = compileEmail'
    , quotePat = error "Email is not a pattern; use `emailToText` instead"
    , quoteDec = error "email is not supported at top-level"
    , quoteType = error "email is not supported as a type"
    }
  where
    compileEmail' :: String -> Q Exp
    compileEmail' s = case mkEmail (T.pack s) of
      Nothing -> fail ("Invalid Email: " <> s <> ". Make sure you aren't wrapping the email in quotes.")
      Just email -> [|$(lift email)|]


-- Dollar. Demonstrates a newtype corresponding to a specific postgres type
newtype Dollar = Dollar { unDollar :: Centi }
  deriving stock (Show)
  deriving newtype (PersistField)
  deriving newtype (Eq, Ord, Num, Fractional, Real, RealFrac)

instance PersistFieldSql Dollar where
  sqlType _ = SqlOther "dollar"


-- PurchaseKind. Haskell ADT corresponding with an enum in postgres
data PurchaseKind
  = Cash
  | Credit
  | Debit
  | ACH
  | Wire
  deriving stock (Show, Read, Eq, Ord)

$(derivePostgresEnum ''PurchaseKind "purchase_kind")


-- Value. This is Aeson's Value (representing a JSON blob), not Esqueleto's Value
instance PersistField Aeson.Value where
  toPersistValue value = PersistText $ cs $ Aeson.encode value
  fromPersistValue (PersistText t) = case Aeson.eitherDecode (cs t) of
    Left s -> Left $ "Error decoding into Value; received " <> t <> " error: " <> T.pack s
    Right v -> Right v
  fromPersistValue (PersistByteString bs) = case Aeson.eitherDecode (cs bs) of
    Left s -> Left $ "Error decoding into Value; received " <> cs bs <> " error: " <> T.pack s
    Right v -> Right v
  fromPersistValue x = Left . T.pack $ "Value: When expecting PersistByteString/PersistText, received: " ++ show x

instance PersistFieldSql Aeson.Value where
  sqlType _ = SqlOther "jsonb"
