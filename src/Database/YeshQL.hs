{-#LANGUAGE TemplateHaskell #-}
module Database.YeshQL
where

import Language.Haskell.TH
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)

mkQuery :: String -> Q Exp
mkQuery q =
    let items = words q
        params = catMaybes . map (\x -> if (":" `isPrefixOf` x) then Just x else Nothing) $ items
        items' = map (\x -> if (":" `isPrefixOf` x) then "?" else x) items
        q' = unwords items'
    in [|(q', params)|]
