{-# LANGUAGE TemplateHaskell #-}
module Sound.Tidal.ParamTemplates where

import Control.Monad
import Data.Word (Word8)
import Language.Haskell.TH

import Sound.Tidal.Pattern

data ParamOpts = NoBus | Alias String
                   deriving ( Eq )

mkFunc :: String -> Q Type -> Q Exp -> Q [Dec]
mkFunc nameS qType val = (:) <$> sigD name qType <*> [d| $(varP name) = $(val) |]
  where name = mkName nameS

mkParam :: Name -> String -> [ParamOpts] -> Q [Dec]
mkParam pTypeName pName opts = concat <$> sequence ([p, pt, pc, pct, pb, pr] ++ as)
  where
    pType = (pure . ConT) pTypeName :: Q Type
    p   = mkFunc pName [t| Pattern $(pType) -> ControlPattern |]
                 [e| param pName |]
    pt  = mkFunc (pName ++ "Take") [t| String -> [$(pType)] -> ControlPattern |]
                 [e| \name xs -> pStateList pName name xs |]
    pc  = if pTypeName == ''Double || pTypeName == ''Int || pTypeName == ''Note
          then mkFunc (pName ++ "Count") [t| String -> ControlPattern |]
               [e| \name -> pStateF pName name (maybe 0 (+1)) |]
          else [d| |]
    pct = if pTypeName == ''Double || pTypeName == ''Int || pTypeName == ''Note
          then mkFunc (pName ++ "CountTo") [t| String -> Pattern Double -> Pattern ValueMap |]
               [e| \name ipat -> innerJoin $ (\i -> pStateF pName name (maybe 0 ((`mod'` i) . (+1)))) <$> ipat |]
          else [d| |]
    pb  = mkFunc (pName ++ "bus") [t| Pattern Int -> Pattern $(pType) -> ControlPattern |] $
            if   elem NoBus opts
            then [e| \_ _ -> error $ "Control parameter '" ++ pName ++ "' can't be sent to a bus." |]
            else [e| \busid pat -> (param pName pat) # (pI ('^':pName) busid) |]
    pr  = if notElem NoBus opts
          then mkFunc (pName ++ "recv") [t| Pattern Int -> ControlPattern |]
                      [e| \busid -> pI ('^':pName) busid |]
          else [d| |]
    as  = map (mkParamAlias pName) [a | Alias a <- opts]

mkParamAlias :: String -> String -> Q [Dec]
mkParamAlias pName pAlias = concat <$> sequence [p, pb, pr]
  where
    p  = alias pName pAlias
    pb = alias (pName ++ "bus") (pAlias ++ "bus")
    pr = alias (pName ++ "recv") (pAlias ++ "recv")

mkParamF :: String -> [ParamOpts] -> Q [Dec]
mkParamF = mkParam ''Double

mkParamI :: String -> [ParamOpts] -> Q [Dec]
mkParamI = mkParam ''Int

mkParamS :: String -> [ParamOpts] -> Q [Dec]
mkParamS = mkParam ''String

type Data = [Word8]

mkParamX :: String -> [ParamOpts] -> Q [Dec]
mkParamX = mkParam ''Data

alias :: String -> String -> Q [Dec]
alias full short = aliases full [short]

aliases :: String -> [String] -> Q [Dec]
aliases full = concatMapM mkAlias
  where
    concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
    concatMapM f xs = liftM concat (mapM f xs)
    mkAlias :: String -> Q [Dec]
    mkAlias short = do
        fullName <- lookupValueName full
        case fullName
          of Just name -> reify name >>= mkAlias' short
             Nothing -> return []
    mkAlias' :: String -> Info -> Q [Dec]
    mkAlias' short (VarI fullName fullType _) = 
        mkFunc short (return fullType) (varE fullName)
    mkAlias' _ _ = return []
    