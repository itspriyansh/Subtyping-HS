
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Types.Global where

import Data.Text (Text)
import GHC.Generics (Generic)
import Control.Lens (makeFieldsNoPrefix, (^.), (.~))
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Lib
import Types.Person
import Types.City

data Global = Global
  { _personBasic :: !(Maybe PersonBasic)
  , _cityBasic :: !(Maybe CityBasic)
  }
  deriving (Show, Generic)

makeFieldsNoPrefix ''Global
