{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Person where

import Data.Text (Text)
import GHC.Generics (Generic)
import Control.Lens

data Person = Person
  { _name :: !Text,
    _age :: !Int,
    _city :: !Text,
    _aadhar :: !Text,
    _pan :: !Text,
    _ifsc :: !Text,
    _acNum :: !Text
  }
  deriving (Show, Generic)

data PersonBasic = PersonBasic
  { _name :: !Text,
    _age :: !Int,
    _city :: !Text
  }
  deriving (Show, Generic)

makeFieldsNoPrefix ''Person
makeFieldsNoPrefix ''PersonBasic




