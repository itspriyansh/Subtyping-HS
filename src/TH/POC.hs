{-# LANGUAGE TemplateHaskell #-}

module TH.POC where

import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Syntax
import Data.Char (toLower)
import Control.Monad (forM)
import Data.Maybe (catMaybes)

-- Types used for this TH should be a Product type

getTypeInfo :: Name -> Q (Name, [(Name, Type)])
getTypeInfo dType = do
  info <- reifyDatatype dType
  let consL = datatypeCons info
  cons <- case consL of
    [cons'] -> return cons'
    [] -> fail $ "No constructor found for " <> nameBase dType
    _multipleCons -> fail $ "Multiple constructr found for " <> nameBase dType
  normalizeConstructor cons

normalizeConstructor :: ConstructorInfo -> Q (Name, [(Name, Type)])
normalizeConstructor con = do
  fieldNames <- case constructorVariant con of
    RecordConstructor xs -> return xs
    _otherCons -> fail $ show _otherCons <> " found for " <> nameBase (constructorName con)
  return (constructorName con, zip fieldNames (constructorFields con))

-- data XY = X | Y

-- >>> runQ [| \ _ _ -> 1 |]
-- LamE [WildP,WildP] (LitE (IntegerL 1))

gen :: Int -> String -> Exp
gen n str = LamE (replicate n WildP) (LitE (StringL str))

gen' :: Lift a => Int -> a -> Q Exp
gen' n str = LamE (replicate n WildP) <$> lift str

-- >>> runQ (gen' 20 "d") >>= return . pprint
-- "\\_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ -> ['d']"
--

genPrintFun :: Name -> Name -> DecQ
genPrintFun dType dSubtype = do
  let printFunBody = NormalB (AppE (VarE $ mkName "return") (LitE $ StringL $ "Dynamic Lens " <>  nameBase dType <> " " <> nameBase dSubtype))
      printFun = FunD (mkName "printSubtype") [Clause [WildP, WildP] printFunBody []]
  return printFun

genProjectAuxFun :: Name -> Name -> DecQ
genProjectAuxFun dType dSubtype = do
  (dTypeCons, dTypeFields) <- getTypeInfo dType
  (dSubtypeCons, dSubtypeFields) <- getTypeInfo dSubtype
  typeVarName <- newName (map toLower . nameBase $ dTypeCons)
  fieldsExpr <- forM dSubtypeFields $ getFieldExpr typeVarName dTypeFields
  let projectFunBody = NormalB (RecConE dSubtypeCons fieldsExpr)
      projectFun = FunD (mkName "projectAux") [Clause [VarP typeVarName] projectFunBody []]
  return projectFun
  where
  getFieldExpr :: Name -> [(Name, Type)] -> (Name, Type) -> FieldExpQ
  getFieldExpr typeVarName typeFields (fieldName, fieldType) = do
    let filterField = filter (\ (name', type') -> nameBase fieldName == nameBase name' && fieldType == type') typeFields 
    (targetFieldName, _) <- case filterField of
      [] -> fail $ "Field " <> nameBase fieldName <> " not found or type does not match in " <> nameBase dType
      (x : _) -> return x
    return (fieldName, UInfixE (VarE typeVarName) (VarE $ mkName "^.") (VarE $ mkName $ drop 1 $ nameBase targetFieldName))


genInjectAuxFun :: Name -> Name -> DecQ
genInjectAuxFun dType dSubtype = do
  (dTypeCons, dTypeFields) <- getTypeInfo dType
  (dSubtypeCons, dSubtypeFields) <- getTypeInfo dSubtype
  typeVarName <- newName (map toLower . nameBase $ dTypeCons)
  subtypeVarName <- newName (map toLower . nameBase $ dSubtypeCons)
  fieldsExpr <- forM dSubtypeFields $ getFieldExpr subtypeVarName dTypeFields
  let injectFunBody = NormalB (RecUpdE (VarE typeVarName) (catMaybes fieldsExpr))
      injectFun = FunD (mkName "injectAux") [Clause [VarP typeVarName, VarP subtypeVarName] injectFunBody []]
  return injectFun
  where
  getFieldExpr :: Name -> [(Name, Type)] -> (Name, Type) -> Q (Maybe FieldExp)
  getFieldExpr subtypeVarName typeFields (fieldName, fieldType) = do
    let filterField = filter (\ (name', type') -> nameBase fieldName == nameBase name' && fieldType == type') typeFields 
        targetField = case filterField of
          [] -> Nothing
          (x : _) -> Just x
        fieldMapping (targetFieldName, _) = return (targetFieldName, UInfixE (VarE subtypeVarName) (VarE $ mkName "^.") (VarE $ mkName $ drop 1 $ nameBase fieldName))
    mapM fieldMapping targetField

genProjectFun :: Name -> Name -> DecQ
genProjectFun dType dSubtype = do
  let globalType = mkName "Global"
  typeVarName <- newName (map toLower . nameBase $ dType)
  globalVarName <- newName (map toLower . nameBase $ globalType)
  subtypeVarName <- newName (map toLower . nameBase $ dSubtype)
  fieldExprs <- getFieldExpr globalType subtypeVarName
  let projectFunStmts =
        [ LetS [ValD (VarP subtypeVarName) (NormalB $ AppE (VarE . mkName $ "projectAux") (VarE typeVarName)) []]
        , NoBindS $ AppE (VarE $ mkName "TS.modify'") (LamE [VarP globalVarName] (RecUpdE (VarE globalVarName) fieldExprs))
        ]
      projectFunBody = NormalB (DoE projectFunStmts)
      projectFun = FunD (mkName "project") [Clause [VarP typeVarName] projectFunBody []]
  return projectFun
  where
  getFieldExpr :: Name -> Name -> Q [FieldExp]
  getFieldExpr globalType subtypeVarName = do
    (_, fieldsInfo) <- getTypeInfo globalType
    let filterField = filter (matchType dSubtype . snd) fieldsInfo
        targetField = case filterField of
          [] -> Nothing
          (x : _) -> Just x
        fieldMapping (targetFieldName, VarT _) = return [(targetFieldName, VarE subtypeVarName)]
        fieldMapping (targetFieldName, AppT _ _) = return [(targetFieldName, AppE (ConE . mkName $ "Just") (VarE subtypeVarName))]
        fieldMapping (_, _) = return []
    maybe (return []) fieldMapping targetField

  matchType :: Name -> Type -> Bool
  matchType x (ConT n) = x == n
  matchType x (AppT t1 t2) = matchTypeNameBase (mkName "Maybe") t1 && matchType x t2
  matchType _ _ = False

  matchTypeNameBase :: Name -> Type -> Bool
  matchTypeNameBase x (ConT n) = nameBase x == nameBase n
  matchTypeNameBase _ _ = False

genInjectFun :: Name -> Name -> DecQ
genInjectFun dType dSubtype = do
  let globalType = mkName "Global"
  typeVarName <- newName (map toLower . nameBase $ dType)
  globalVarName <- newName (map toLower . nameBase $ globalType)
  subtypeVarName <- newName (map toLower . nameBase $ dSubtype)
  subTypeValue <- getValueFromGlobal globalType globalVarName
  injectResVar <- newName (map toLower . nameBase $ dType)
  let injectFunStmts =
        [ BindS (VarP globalVarName) (VarE . mkName $ "TS.get")
        , LetS
            [ ValD (VarP subtypeVarName) (NormalB subTypeValue) []
            , ValD (VarP injectResVar) (NormalB $ AppE (AppE (VarE . mkName $ "injectAux") (VarE typeVarName)) (VarE subtypeVarName)) []
            ]
        , NoBindS $ AppE (VarE . mkName $ "return") (VarE injectResVar)
        ]
      injectFunBody = NormalB (DoE injectFunStmts)
      injectFun = FunD (mkName "inject") [Clause [VarP typeVarName] injectFunBody []]
  return injectFun
  where
  getValueFromGlobal :: Name -> Name -> ExpQ
  getValueFromGlobal globalType globalVarName = do
    (_, fieldsInfo) <- getTypeInfo globalType
    let filterField = filter (matchType dSubtype . snd) fieldsInfo
    (targetFieldName, _) <- case filterField of
      [] -> fail (nameBase dSubtype <> " not found in " <> nameBase globalType)
      (x : _) -> return x
    return $ AppE (VarE . mkName $ "fromJust") (UInfixE (VarE globalVarName) (VarE . mkName $ "^.") (VarE $ mkName $ drop 1 $ nameBase targetFieldName))
  
  matchType :: Name -> Type -> Bool
  matchType x (ConT n) = x == n
  matchType x (AppT t1 t2) = matchTypeNameBase (mkName "Maybe") t1 && matchType x t2
  matchType _ _ = False

  matchTypeNameBase :: Name -> Type -> Bool
  matchTypeNameBase x (ConT n) = nameBase x == nameBase n
  matchTypeNameBase _ _ = False

genSubtypeLensInstance :: Name -> Name -> Q [Dec]
genSubtypeLensInstance dType dSubtype = do
  printFun <- genPrintFun dType dSubtype
  projectAuxFun <- genProjectAuxFun dType dSubtype
  injectAuxFun <- genInjectAuxFun dType dSubtype
  projectFun <- genProjectFun dType dSubtype
  injectFun <- genInjectFun dType dSubtype
  let subTypeLensClass = ConT $ mkName "SubtypeLens"
      personType = ConT dType
      personBasicType = ConT dSubtype
  return [InstanceD Nothing [] (AppT (AppT subTypeLensClass personType) personBasicType) [printFun, projectAuxFun, injectAuxFun, projectFun, injectFun]]
