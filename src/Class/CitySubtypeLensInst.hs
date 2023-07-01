{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Class.CitySubtypeLensInst where

import Class.SubtypeLens
import TH.POC
import Control.Lens
import Types.Global
import Types.City
import qualified Control.Monad.Trans.State as TS
import Data.Maybe (fromJust)

genSubtypeLensInstance ''City ''CityBasic
