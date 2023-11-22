{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Data.SerDoc.TH
( deriveSerializable
, deriveHasInfo
, deriveSerDoc
)
where

import Data.SerDoc.Info
import Data.SerDoc.Class

import Data.List
import Data.Proxy
import Language.Haskell.TH
import Data.Char
import Control.Monad.State

-- * Deriving 'HasInfo' and 'Serializable' with Template Haskell

strippedFieldName :: Name -> Name -> String
strippedFieldName tyName fieldName =
  let tyStr = nameBase tyName
      fieldStr = nameBase fieldName
      lcfirst [] = []
      lcfirst (x:xs) = toLower x : xs
      tyStrLC = lcfirst tyStr
  in
    if tyStrLC `isPrefixOf` fieldStr then
      drop (length tyStrLC) fieldStr
    else
      fieldStr

-- | Derive a 'HasInfo' instance for the given codec and type.
-- Currently only supports record types.
-- A matching 'Serializable' instance must serialize record fields in the order
-- they are declared in the source code, without any additional separators,
-- padding, or envelope around or between them. If your serializer does not meet
-- these requirements, you must write a custom 'HasInfo' instance instead.
deriveHasInfo :: Name -> Name -> DecsQ
deriveHasInfo codecName typeName = do
  reify typeName >>= \case
    TyConI (DataD [] tyName tyVars Nothing [RecC _ fields] []) -> do
      [d| instance HasInfo $(conT codecName) $(foldl appT (conT tyName) [ varT (tyVarName bndr) | bndr <- tyVars ]) where
            info codec _ =
              compoundField
                $(litE (stringL (nameBase tyName)))
                $(listE
                    [ [| ( $(litE (stringL (strippedFieldName tyName fieldName)))
                         , info codec (Proxy :: Proxy $(return fieldTy))
                         )
                      |]
                    | (fieldName, _, fieldTy) <- fields
                    ]
                  )
        |]
    x ->
      error $ "Unsupported data type " ++ show typeName ++ ": " ++ show x

(<<>>) :: (Applicative m, Monoid a) => m a -> m a -> m a
a <<>> b = (<>) <$> a <*> b

-- | Derive a 'Serializable' instance for the given codec and type.
-- Currently only supports record types.
-- The generated instance will serialize record fields in the order
-- they are declared in the source code, without any additional separators,
-- padding, or envelope around or between them, making it compatible with
-- 'deriveHasInfo'. (See also 'deriveSerDoc'.)
deriveSerializable :: Name -> Name -> DecsQ
deriveSerializable codecName typeName = do
  reify typeName >>= \case
    TyConI (DataD [] tyName tyVars Nothing [RecC conName fields] []) -> do
      [d| instance
            Codec $(conT codecName) =>
            Serializable $(conT codecName) $(foldl appT (conT tyName) [ varT (tyVarName bndr) | bndr <- tyVars ]) where
              encode p ctx item =
                $(foldr1 (\a b -> varE '(<<>>) `appE` a `appE` b)
                  [ [| encode p ctx ($(varE fieldName) item) |]
                  | (fieldName, _, _) <- fields
                  ]
                 )
              decodeM p ctx =
                runStateT
                  (
                    $(foldApplicative
                        (conE conName)
                        [ [| StateT $ decodeM p ctx |] | _ <- fields ]
                     )
                  )
        |]
    x ->
      error . show $ x

-- | Derive both a 'HasInfo' instance and a matching 'Serializable' instance,
-- combining 'deriveHasInfo' and 'deriveSerializable'.
deriveSerDoc :: Name -> Name -> DecsQ
deriveSerDoc codecName typeName =
  (++) <$> deriveHasInfo codecName typeName
       <*> deriveSerializable codecName typeName

-- <$> :: (a -> b) -> f a -> f b
-- <*> :: f (a -> b) -> f a -> f b
foldApplicative :: ExpQ -> [ExpQ] -> ExpQ
foldApplicative initial [] = [| pure $initial |]
foldApplicative initial [x] = [| $initial <$> $x |]
foldApplicative initial (x:xs) =
  foldl (\a b -> [| $a <*> $b |]) [| $initial <$> $x |] xs

tyVarName :: TyVarBndr a -> Name
tyVarName (PlainTV n _) = n
tyVarName (KindedTV n _ _) = n