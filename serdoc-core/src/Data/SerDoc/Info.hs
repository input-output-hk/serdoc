{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.SerDoc.Info
where

import Data.Data
import Data.Word
import Language.Haskell.TH.Syntax (Lift)

-- * Documentation annotation types

-- | Used to add descriptions via @ANN@ pragmas. This is necessary because
-- Template Haskell cannot find Haddock comments attached to constructors inside
-- associated types, but it can find annotations on those same constructors.
newtype Description = Description { descriptionParagraphs :: [String] }
  deriving newtype (Show, Read, Eq, Semigroup, Monoid)
  deriving (Data, Typeable, Lift)

-- * 'FieldInfo' and related types

-- | Describes the serialization format of a field.
data FieldInfo codec
  = AnnField !String !(FieldInfo codec)
    -- ^ Adds an annotation to a field.
  | BasicField !BasicFieldInfo
    -- ^ A simple field, with a named type and a size.
  | EnumField !EnumFieldInfo
    -- ^ An enum field, which reports labels and values for the possible values.
  | CompoundField !(CompoundFieldInfo codec)
    -- ^ A field that is composed out of multiple sub-fields, encoded
    -- sequentially.
  | ChoiceField !(ChoiceFieldInfo codec)
    -- ^ A list of alternatives, to be picked based on a given choice condition.
  | ListField !(ListFieldInfo codec)
    -- ^ A list of values, encoded sequentially. The length must be encoded
    -- separately, and can be referenced from a length expression.
  | AliasField !(AliasFieldInfo codec)
    -- ^ Adds an alternative name (alias) to a field type.
  | SumField !(SumFieldInfo codec)
    -- ^ A list of named alternatives. TODO: this is probably redundant and may
    -- not actually work.
  deriving (Show, Eq)

data BasicFieldInfo =
  BasicFieldInfo
    { basicFieldType :: !String
    , basicFieldSize :: !FieldSize
    }
  deriving (Show, Eq)

data EnumFieldInfo =
  EnumFieldInfo
    { enumFieldType :: !String
    , enumFieldSize :: !FieldSize
    , enumFieldValues :: ![(Int, String)]
    }
  deriving (Show, Eq)

data AliasFieldInfo codec =
  AliasFieldInfo
    { aliasFieldName :: !String
    , aliasFieldTarget :: !(FieldInfo codec)
    }
  deriving (Show, Eq)

data CompoundFieldInfo codec =
  CompoundFieldInfo
    { compoundFieldType :: !String
    , compoundFieldSubfields :: ![SubfieldInfo codec]
    }
  deriving (Show, Eq)

data SumFieldInfo codec =
  SumFieldInfo
    { sumFieldType :: !String
    , sumFieldAlternatives :: ![(String, FieldInfo codec)]
    }
  deriving (Show, Eq)

data ListFieldInfo codec =
  ListFieldInfo
    { listSize :: !FieldSize
    , listElemInfo :: !(FieldInfo codec)
    }
  deriving (Show, Eq)

data SubfieldInfo codec =
  SubfieldInfo
    { subfieldName :: !String
    , subfieldInfo :: !(FieldInfo codec)
    }
  deriving (Show, Eq)

data ChoiceCondition
  = IndexField !String
  | IndexFlag !String Word32
  deriving (Show, Eq)

data ChoiceFieldInfo codec =
  ChoiceFieldInfo
    { choiceCondition :: !ChoiceCondition
    , choiceFieldAlternatives :: ![FieldInfo codec]
    }
  deriving (Show, Eq)

annField :: String -> FieldInfo codec -> FieldInfo codec
annField = AnnField

basicField :: String -> FieldSize -> FieldInfo codec
basicField ty size = BasicField $ BasicFieldInfo ty size

enumField :: String -> FieldSize -> [(Int, String)] -> FieldInfo codec
enumField ty size values = EnumField $ EnumFieldInfo ty size values

enumField_ :: String
           -> [String]
           -> FieldInfo codec
enumField_ ty values = enumField ty EnumSize (zip [0,1..] values)

aliasField :: String -> FieldInfo codec -> FieldInfo codec
aliasField name ty = AliasField $ AliasFieldInfo name ty

compoundField :: String -> [(String, FieldInfo codec)] -> FieldInfo codec
compoundField ty subfields =
  CompoundField $
    CompoundFieldInfo
      ty
      [ SubfieldInfo name i
      | (name, i) <- subfields
      ]

choiceField :: ChoiceCondition -> [FieldInfo codec] -> FieldInfo codec
choiceField cond subfields =
  ChoiceField $
    ChoiceFieldInfo
      cond
      subfields

sumField :: String -> [(String, FieldInfo codec)] -> FieldInfo codec
sumField name alternatives =
  SumField $
    SumFieldInfo
      name
      alternatives

listField :: FieldSize -> FieldInfo codec -> FieldInfo codec
listField lengthExpr elemInfo =
  ListField $
    ListFieldInfo
      lengthExpr
      elemInfo

-- * Field sizes

data FieldSize
  = FixedSize !Int -- ^ Exactly this size
  | EnumSize -- ^ The default enum size for the codec
  | VarSize !String -- ^ Size given by a named variable from the context
  | BinopSize !FieldSizeBinop !FieldSize !FieldSize -- ^ Binary operation
  | RangeSize !FieldSize !FieldSize -- ^ Min/max range
  | UnknownSize -- ^ Size is entirely unknown
  deriving (Show, Eq)

knownSize :: FieldSize -> Maybe Int
knownSize (FixedSize i) = Just i
knownSize VarSize {} = Nothing
knownSize (BinopSize FSPlus a b) = (+) <$> knownSize a <*> knownSize b
knownSize (BinopSize FSMul a b) = (*) <$> knownSize a <*> knownSize b
knownSize (BinopSize FSMax a b) = max <$> knownSize a <*> knownSize b
knownSize (BinopSize FSMin a b) = min <$> knownSize a <*> knownSize b
knownSize _ = Nothing

data FieldSizeBinop
  = FSPlus
  | FSMul
  | FSMax
  | FSMin
  deriving (Show, Eq)

