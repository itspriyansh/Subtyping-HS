
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.City where

import Data.Text (Text)
import GHC.Generics (Generic)
import Control.Lens (makeFieldsNoPrefix, (^.), (.~))
import TH.POC
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Lib

data City = City
  { _name :: !Text
  , _state :: !Text
  , _country :: !Text
  , _tier :: !Text
  , _pincode :: !Text
  , _area :: !Int
  , _capital :: !Bool
  }
  deriving (Show, Generic)

data CityBasic = CityBasic
  { _name :: !Text
  , _state :: !Text
  , _country :: !Text
  , _capital :: !Bool
  }
  deriving (Show, Generic)

makeFieldsNoPrefix ''City
makeFieldsNoPrefix ''CityBasic
