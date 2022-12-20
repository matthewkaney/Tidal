{-# LANGUAGE TemplateHaskell #-}
module Sound.Tidal.ParamTemplates where

import Data.Map.Strict as Map

import Sound.Tidal.Pattern

import Language.Haskell.TH

data ParamOpts = NoBus | Alias String

param :: Valuable a => String -> Pattern a -> ControlPattern
param name = fmap (Map.singleton name . toValue)

mkParam :: Name -> String -> [ParamOpts] -> Q [Dec]
mkParam pTypeName name _ = do
    let pName = mkName name
    let pType = (pure . ConT) pTypeName :: Q Type
    (:) <$> pName `sigD` [t| Pattern $(pType) -> ControlPattern |]
        <*> [d| $(varP pName) = param name |]

mkParamF :: String -> [ParamOpts] -> Q [Dec]
mkParamF = mkParam ''Double

mkParamI :: String -> [ParamOpts] -> Q [Dec]
mkParamI = mkParam ''Int

-- param' :: String -> Q [Dec]
-- param' pName = [d| $(mkName pName) :: Pattern Double -> ControlPattern
--                   $(mkName pName) = pF pName
--                   $(pTake) :: String -> [Double] -> ControlPattern
--                   $(pTake) name xs = pStateListF pName name xs
--                   beginCount :: String -> ControlPattern
--                   beginCount name = pStateF "begin" name (maybe 0 (+1))
--                   beginCountTo :: String -> Pattern Double -> Pattern ValueMap
--                   beginCountTo name ipat = innerJoin $ (\i -> pStateF "begin" name (maybe 0 ((`mod'` i) . (+1)))) <$> ipat
--                 |]
--               where p = varP $ mkName pName
--                     pTake = varP $ mkName (pName ++ "Take")

alias :: String -> String -> Q [Dec]
alias short full = [d| $(varP $ mkName short) = $(varE $ mkName full) |]