{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing -Wno-missing-signatures -Wno-type-defaults #-}

module Iohk.Types
  ( ApplicationVersion(..)
  , Environment(..)
  ) where

import           Data.Text (Text)
import           Data.Yaml             (FromJSON, ToJSON)
import           GHC.Generics (Generic)

newtype ApplicationVersion = ApplicationVersion { getApplicationVersion :: Text } deriving (FromJSON, Show, Eq, Generic, ToJSON)


data Environment
  = Any               -- ^ Wildcard or unspecified, depending on context.
  | Benchmark
  | Production
  | Staging
  | Testnet
  | Preview
  | Preprod
  | Development
  | DevOps
  | Nightly
  | ITNBC
  | ITNRW
  | MainnetFlight
  | ShelleyTestnet
  | MainnetCatalyst
  deriving (Bounded, Eq, Enum, Generic, Read, Show)

instance FromJSON Environment
