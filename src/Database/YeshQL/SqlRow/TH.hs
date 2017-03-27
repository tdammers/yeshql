{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE QuasiQuotes #-}
{-#LANGUAGE LambdaCase #-}
module Database.YeshQL.SqlRow.TH
where

import Database.YeshQL.SqlRow.Class
import Database.HDBC (fromSql, toSql)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Generics.SYB.WithClass.Derive (typeInfo)

makeSqlRow :: Name -> Q [Dec]
makeSqlRow entityName = do
    (TyConI d) <- reify entityName
    (typeName, _, constructors) <- typeInfo d

    (constructorName, fieldNames, fieldTypes) <-
        case constructors of
            [(constructorName, _, Just fieldNames, fieldTypes)] ->
                return (constructorName, fieldNames, fieldTypes)
            _ -> fail "Unsuitable type for deriving SqlRow"

    [d|
        instance ToSqlRow $(conT typeName) where
            toSqlRow entity =
                $(listE $ map (toSqlRowField 'entity) fieldNames)

        instance FromSqlRow $(conT typeName) where
            parseSqlRow = Parser $ \case
                $(foldr
                    (\x xs -> infixP x '(:) xs)
                    (varP $ mkName "remaining")
                    (map fromSqlPatternItem fieldNames)
                 ) ->
                    return
                        ( $(foldl1 appE $
                            conE constructorName : map fromSqlPatternArg fieldNames)
                        , remaining
                        )
                _ -> fail $ "Invalid SQL for " ++ $(litE . stringL . nameBase $ typeName) |]
    where
        toSqlRowField :: Name -> Name -> ExpQ
        toSqlRowField entityName fieldName =
            appE [|toSql|] $ appE (varE fieldName) (varE entityName)

        fromSqlPatternItem :: Name -> PatQ
        fromSqlPatternItem fieldName =
            varP (mkName $ "sql_" ++ nameBase fieldName)

        fromSqlPatternArg :: Name -> ExpQ
        fromSqlPatternArg fieldName =
            appE
                (varE (mkName "fromSql"))
                (varE (mkName $ "sql_" ++ nameBase fieldName))
