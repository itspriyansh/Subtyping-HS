{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Class.PersonSubtypeLensInst where

import Class.SubtypeLens
import TH.POC
import Control.Lens
import Types.Global
import Types.Person

genSubtypeLensInstance ''Person ''PersonBasic

example :: PersonBasic
example = PersonBasic {_name = "Abhishek", _age = 24, _city = "Prayagraj"}

person :: Person
person = Person "Priyansh" 22 "Bengaluru" "24135413251" "31425123" "JUSP3242" "23513251"

-- >>> projectAux person
-- PersonBasic {_name = "Priyansh", _age = 22, _city = "Bengaluru"}

-- >>> injectAux person example
-- Person {_name = "Abhishek", _age = 24, _city = "Prayagraj", _aadhar = "24135413251", _pan = "31425123", _ifsc = "JUSP3242", _acNum = "23513251"}

