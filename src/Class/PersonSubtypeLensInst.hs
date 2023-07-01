{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Class.PersonSubtypeLensInst where

import Class.SubtypeLens
import TH.POC
import Control.Lens
import Types.Global
import Types.Person
import qualified Types.City as TC
import qualified Control.Monad.Trans.State as TS
import Class.CitySubtypeLensInst
import Data.Maybe (fromJust)

genSubtypeLensInstance ''Person ''PersonBasic

example :: PersonBasic
example = PersonBasic {_name = "Abhishek", _age = 24, _city = "Prayagraj"}

person :: Person
person = Person "Priyansh" 22 "Bengaluru" "24135413251" "31425123" "JUSP3242" "23513251"

city' :: TC.City
city' = TC.City "Bengaluru" "Karnataka" "Country" "One" "590030" 273298 True

-- >>> projectAux person
-- PersonBasic {_name = "Priyansh", _age = 22, _city = "Bengaluru"}

-- >>> injectAux person example
-- Person {_name = "Abhishek", _age = 24, _city = "Prayagraj", _aadhar = "24135413251", _pan = "31425123", _ifsc = "JUSP3242", _acNum = "23513251"}

g1 :: Global
g1 = TS.execState (project person >> project city') (Global Nothing Nothing)
-- >>> g1
-- Global {_personBasic = Just (PersonBasic {_name = "Priyansh", _age = 22, _city = "Bengaluru"}),
-- _cityBasic = Just (CityBasic {_name = "Bengaluru", _state = "Karnataka", _country = "Country", _capital = True})}

g3 :: Person
g3 = TS.evalState (inject person) (personBasic ?~ example $ g1)
-- >>> g3
-- Person {_name = "Abhishek", _age = 24, _city = "Prayagraj", _aadhar = "24135413251", _pan = "31425123", _ifsc = "JUSP3242", _acNum = "23513251"}
