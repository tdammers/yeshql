{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE QuasiQuotes #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE CPP #-}
module Database.YeshQL.SqlRow.TH
( makeSqlRow
)
where

import Database.YeshQL.SqlRow.Class
import Database.HDBC (fromSql, toSql)
import Language.Haskell.TH
import Language.Haskell.TH.Quote

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

type Constructor =
  (Name,         -- Name of the constructor
   Int,          -- Number of constructor arguments
   Maybe [Name], -- Name of the field selector, if any
   [Type])       -- Type of the constructor argument

typeInfo :: Dec
         -> Q (Name,            -- Name of the datatype
               [Name],          -- Names of the type parameters
               [Constructor])   -- The constructors
typeInfo d
        = case d of
#if MIN_VERSION_template_haskell(2,11,0)
            DataD    _ n ps _ cs _ -> return (n, map varName ps, map conA cs)
            NewtypeD _ n ps _ c  _ -> return (n, map varName ps, [conA c])
#else
            DataD    _ n ps cs _ -> return (n, map varName ps, map conA cs)
            NewtypeD _ n ps c  _ -> return (n, map varName ps, [conA c])
#endif
            _ -> error ("derive: not a data type declaration: " ++ show d)
        where
            conA (NormalC c xs) = (c, length xs, Nothing, map snd xs)
            conA (InfixC x1 c x2) = conA (NormalC c [x1, x2])
            conA (ForallC _ _ c) = conA c
            conA (RecC c xs) =
                    let getField (n, _, _) = n
                        getType  (_, _, t) = t
                        fields = map getField xs
                        types  = map getType xs
                    in (c, length xs, Just fields, types)
            varName (PlainTV n) = n
            varName (KindedTV n _) = n
