{-# LANGUAGE TemplateHaskell #-}

module Language.Haskellish.TH where

import Language.Haskell.TH
-- import Control.Monad
import Data.Map as Map
-- import Data.Set as Set

grabExp :: String -> Q Exp
grabExp x = varE $ mkName x

-- given a list of names of functions, make a map of that info
-- eg. as a splice that becomes a String: $(grabInfoMap ["f","g","h"] >>= (stringE . show))
grabInfoMap :: [String] -> Q (Map String Info)
grabInfoMap = sequence . Map.fromList . fmap (\x -> (x,reify $ mkName x))

{- infoToType :: Info -> Type
infoToType (VarI _ x _) = x -}

-- given a type, return a representation of it as [String] where
-- concatenating the strings produces a string that could be a legal name for a definition
-- and where each items from the list is also a legal name for a definition
{-
signature :: Type -> [String]
signature (AppT (AppT ArrowT x) y) = (concat $ signature x) : signature y
signature (AppT ListT x) = ["_LIST" ++ concat (signature x)]
signature (AppT (ConT x) (ConT y)) = [nameBase x ++ nameBase y]
signature (AppT x y) = nameForType x ++ "_OPEN_" ++ nameForType y ++ "_CLOSE_"
signature (ConT x) = nameBase x

-- given a map from names to Infos, generate a map from type-strings to names with that type-string
infoMapToTypeMap :: Map String Info -> Map String [String]
infoMapToTypeMap x = let
  allNamesAndTypes = fmap (nameForType . infoToType) x
  setOfAllTypes = (Set.fromList $ Map.elems allNamesAndTypes) :: Set String
  in Map.fromSet (\t -> Map.keys $ Map.filter (==t) allNamesAndTypes) setOfAllTypes
-}
