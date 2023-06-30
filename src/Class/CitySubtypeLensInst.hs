{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Class.CitySubtypeLensInst where

import Class.SubtypeLens
import TH.POC
import Control.Lens
import Types.Global
import Types.City

genSubtypeLensInstance ''City ''CityBasic
