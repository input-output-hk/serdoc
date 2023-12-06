{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.SerDoc.Class
where

import Data.SerDoc.Info

import Data.Kind
import Data.List
import Data.Map ( Map )
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Typeable
import Data.Word

-- * Typeclasses

-- | Abstracts over an individual serializer / deserializer, a.k.a., a
-- \"codec\". A codec typically provides a serializer, deserializer, and
-- metadata for each serializable type; however, for various reasons, the
-- 'Codec' typeclass itself only captures the associated types that are involved
-- in serializing and deserializing.
class Codec codec where
  -- | The 'Monad' in which encoding can happen.
  type MonadEncode codec :: Type -> Type

  -- | The 'Monad' in which decoding can happen.
  type MonadDecode codec :: Type -> Type

  -- | Unless explicitly declared otherwise, enum fields will be encoded as
  -- this type.
  type DefEnumEncoding codec :: Type
  type DefEnumEncoding codec = Word16

-- | Serialization and deserialization API for a 'Codec'.
class Codec codec => Serializable codec a where
  -- | Encode / serialize a value.
  encode :: Proxy codec -> a -> MonadEncode codec ()

  -- | Decode / deserialize a value.
  decode :: Proxy codec -> MonadDecode codec a

-- | Serialization metadata for a 'Codec'.
class Codec codec => HasInfo codec a where
  info :: Proxy codec -> Proxy a -> FieldInfo codec

-- * Helpers For Writing Instances

-- | Newtype wrapper for deriving / defining 'HasInfo' and 'Serializable'
-- instances for enum types.
newtype ViaEnum a = ViaEnum { viaEnum :: a }
  deriving newtype (Show)

instance ( Enum a
         , Bounded a
         , Typeable a
         , Show a
         , Codec codec
         , HasInfo codec (DefEnumEncoding codec)
         ) => HasInfo codec (ViaEnum a)
  where
    info pCodec _ =
      enumInfo pCodec (Proxy @a) (Proxy @(DefEnumEncoding codec))

instance ( Enum a
         , Bounded a
         , Codec codec
         , Integral (DefEnumEncoding codec)
         , Num (DefEnumEncoding codec)
         , Monad (MonadEncode codec)
         , Monad (MonadDecode codec)
         , Serializable codec (DefEnumEncoding codec)
         ) => Serializable codec (ViaEnum a)
  where
    encode pCodec (ViaEnum x) = encodeEnum pCodec (Proxy @(DefEnumEncoding codec)) x
    decode pCodec = ViaEnum <$> decodeEnum pCodec (Proxy @(DefEnumEncoding codec))

enumInfo :: forall codec a n.
            ( Typeable a
            , Show a
            , Enum a
            , Bounded a
            , Codec codec
            , HasInfo codec n
            , HasInfo codec (DefEnumEncoding codec)
            )
         => Proxy codec
         -> Proxy a
         -> Proxy n
         -> FieldInfo codec
enumInfo pCodec _ pN =
    enumField
      (getTypeName $ Proxy @a)
      (fieldSize $ info pCodec pN)
      [ (fromEnum val, show val) | val <- [minBound .. maxBound :: a] ]

encodeEnum :: forall codec n a.
              ( Enum a
              , Bounded a
              , Codec codec
              , Num n
              , Serializable codec n
              )
           => Proxy codec
           -> Proxy n
           -> a
           -> MonadEncode codec ()
encodeEnum pCodec _ x = do
  let i :: n = fromIntegral . fromEnum $ x
  encode pCodec i

decodeEnum :: forall codec n a.
               ( Enum a
               , Bounded a
               , Codec codec
               , Integral n
               , Serializable codec n
               , Monad (MonadDecode codec)
               )
            => Proxy codec
            -> Proxy n
            -> (MonadDecode codec) a
decodeEnum pCodec _ = do
  (i :: n) <- decode pCodec
  return (toEnum . fromIntegral $ i)

getTypeName :: Typeable a => Proxy a -> String
getTypeName = tyConName . typeRepTyCon . typeRep


-- * Helpers For Dealing With 'FieldInfo' And 'Field Size'

fieldType :: forall codec.
             HasInfo codec Word32
          => FieldInfo codec -> String
fieldType (AnnField _ fi) = fieldType fi
fieldType (BasicField fi) = basicFieldType fi
fieldType (EnumField fi) = enumFieldType fi ++ " = " ++ fieldType (info @codec @Word32 Proxy Proxy)
fieldType (CompoundField fi) = compoundFieldType fi
fieldType (ChoiceField fi) = intercalate " | " $ map fieldType (choiceFieldAlternatives fi)
fieldType (ListField fi) = "[" ++ fieldType (listElemInfo fi) ++ "]"
fieldType (AliasField fi) = aliasFieldName fi ++ " = " ++ fieldType (aliasFieldTarget fi)
fieldType (SumField fi) = sumFieldType fi

shortFieldType :: FieldInfo codec -> String
shortFieldType (AnnField _ fi) = shortFieldType fi
shortFieldType (BasicField fi) = basicFieldType fi
shortFieldType (EnumField fi) = enumFieldType fi
shortFieldType (CompoundField fi) = compoundFieldType fi
shortFieldType (ChoiceField fi) = intercalate " | " $ map shortFieldType (choiceFieldAlternatives fi)
shortFieldType (ListField fi) = "[" ++ shortFieldType (listElemInfo fi) ++ "]"
shortFieldType (AliasField fi) = aliasFieldName fi
shortFieldType (SumField fi) = sumFieldType fi

-- | Reduce a 'FieldInfo' to report only the relevant information for a known
-- constructor.
infoOf :: String -> FieldInfo codec -> FieldInfo codec
infoOf name (AnnField _ fi) = infoOf name fi
infoOf name (EnumField fi) =
  EnumField fi
    { enumFieldValues =
        [ (i, n)
        | (i, n) <- enumFieldValues fi
        , n == name
        ]
    }
infoOf name (SumField fi) =
  SumField fi
    { sumFieldAlternatives =
        [ (n, i)
        | (n, i) <- sumFieldAlternatives fi
        , n == name
        ]
    }
infoOf _ fi = fi

formatPath :: [String] -> String
formatPath = intercalate "." . reverse

scopeFieldSize :: String -> FieldSize -> FieldSize
scopeFieldSize scope (VarSize var) = VarSize (scope ++ "." ++ var)
scopeFieldSize scope (BinopSize op a b) = BinopSize op (scopeFieldSize scope a) (scopeFieldSize scope b)
scopeFieldSize scope (RangeSize a b) = RangeSize (scopeFieldSize scope a) (scopeFieldSize scope b)
scopeFieldSize _ x = x

simplifyFieldSize :: FieldSize -> FieldSize
simplifyFieldSize (RangeSize a b) =
  let a' = simplifyFieldSize a
      b' = simplifyFieldSize b
  in
    if a' == b' then
      a'
    else
      case (a', b') of
        (RangeSize aa' ab', RangeSize ba' bb') ->
          simplifyFieldSize (RangeSize (BinopSize FSMin aa' ba') (BinopSize FSMax ab' bb'))
        (a'', RangeSize ba' bb') ->
          simplifyFieldSize (RangeSize (BinopSize FSMin a'' ba') (BinopSize FSMax a'' bb'))
        _ -> RangeSize a' b'

simplifyFieldSize (BinopSize op a b) =
  let a' = simplifyFieldSize a
      b' = simplifyFieldSize b
  in
    case (a', op, b') of
      (UnknownSize, _, _) -> UnknownSize
      (_, _, UnknownSize) -> UnknownSize

      (FixedSize x, FSPlus, BinopSize FSPlus (FixedSize y) z) ->
        simplifyFieldSize (BinopSize FSPlus (FixedSize (x + y)) z)
      (BinopSize FSPlus z (FixedSize y), FSPlus, FixedSize x) ->
        simplifyFieldSize (BinopSize FSPlus (FixedSize (x + y)) z)
      (RangeSize la ra, _, RangeSize lb rb) ->
        simplifyFieldSize (RangeSize (BinopSize op la lb) (BinopSize op ra rb))
      (RangeSize l r, _, c) ->
        simplifyFieldSize (RangeSize (BinopSize op l c) (BinopSize op r c))
      (x, FSPlus, BinopSize FSPlus y z) ->
        simplifyFieldSize (BinopSize FSPlus (BinopSize FSPlus x y) z)

      (FixedSize x, FSMul, BinopSize FSMul (FixedSize y) z) ->
        simplifyFieldSize (BinopSize FSMul (FixedSize (x + y)) z)
      (BinopSize FSMul z (FixedSize y), FSMul, FixedSize x) ->
        simplifyFieldSize (BinopSize FSMul (FixedSize (x + y)) z)

      (FixedSize x, FSPlus, FixedSize y) -> FixedSize (x + y)
      (FixedSize x, FSMul, FixedSize y) -> FixedSize (x * y)

      (FixedSize x, FSMax, FixedSize y) -> FixedSize (max x y)
      (FixedSize x, FSMin, FixedSize y) -> FixedSize (min x y)

      (FixedSize x, FSPlus, RangeSize lo hi) ->
        simplifyFieldSize (RangeSize (BinopSize FSPlus (FixedSize x) lo) (BinopSize FSPlus (FixedSize x) hi))

      (FixedSize 0, FSPlus, y) -> y
      (x, FSPlus, FixedSize 0) -> x
      (FixedSize 1, FSMul, y) -> y
      (x, FSMul, FixedSize 1) -> x
      (FixedSize 0, FSMin, _) -> FixedSize 0
      (_, FSMin, FixedSize 0) -> FixedSize 0
      (FixedSize 0, FSMax, y) -> y
      (x, FSMax, FixedSize 0) -> x

      _ -> BinopSize op a' b'
simplifyFieldSize x = x

resolveSizeScopes :: forall codec.
                     ( Codec codec
                     , HasInfo codec (DefEnumEncoding codec)
                     )
                  => Proxy codec
                  -> Map String [String]
                  -> FieldSize
                  -> FieldSize
resolveSizeScopes _ env (VarSize name) =
  let name' = maybe name formatPath $ Map.lookup name env
  in VarSize name'
resolveSizeScopes pCodec env (BinopSize op a b) =
  BinopSize op (resolveSizeScopes pCodec env a) (resolveSizeScopes pCodec env b)
resolveSizeScopes pCodec env (RangeSize a b) =
  RangeSize (resolveSizeScopes pCodec env a) (resolveSizeScopes pCodec env b)
resolveSizeScopes pCodec env EnumSize =
  resolveSizeScopes pCodec env (fieldSize $ info pCodec (Proxy @(DefEnumEncoding codec)))
resolveSizeScopes _ _ x = x

fieldSize :: forall codec.
             ( Codec codec
             , HasInfo codec (DefEnumEncoding codec)
             )
          => FieldInfo codec
          -> FieldSize
fieldSize = fieldSizeScoped [] mempty

fieldSizeScoped :: forall codec.
                   ( Codec codec
                   , HasInfo codec (DefEnumEncoding codec)
                   )
                => [String]
                -> Map String [String]
                -> FieldInfo codec
                -> FieldSize
fieldSizeScoped path env (AnnField _ fi) =
  fieldSizeScoped path env fi
fieldSizeScoped path env (AliasField fi) =
  fieldSizeScoped path env (aliasFieldTarget fi)
fieldSizeScoped _ env (BasicField fi) =
  resolveSizeScopes (Proxy @codec) env (basicFieldSize fi)
fieldSizeScoped _ env (EnumField fi) =
  resolveSizeScopes (Proxy @codec) env (enumFieldSize fi)
fieldSizeScoped path env (CompoundField fi) =
  let env' = foldl' (\e sfi -> Map.insert (subfieldName sfi) (subfieldName sfi : path) e) env (compoundFieldSubfields fi)
      qualifiedSubfieldSizes sfi =
        let path' = subfieldName sfi : path
            env'' = Map.insert (subfieldName sfi) path' env'
        in
          fieldSizeScoped path' env'' (subfieldInfo sfi)
  in
    case map qualifiedSubfieldSizes (compoundFieldSubfields fi) of
      [] -> FixedSize 0
      (x:xs) -> simplifyFieldSize $ foldl' (BinopSize FSPlus) x xs
fieldSizeScoped path env (ListField fi) =
  let elemSize = maybe UnknownSize FixedSize $
                  knownSize
                    (fieldSizeScoped path env (listElemInfo fi))
  in
    simplifyFieldSize $
      BinopSize FSMul (listSize fi) elemSize
fieldSizeScoped path env (ChoiceField fi) =
  case map (fieldSizeScoped path env) (choiceFieldAlternatives fi) of
    [] -> FixedSize 0
    (x:xs) -> let maxVal = foldl' (BinopSize FSMax) x xs
                  minVal = foldl' (BinopSize FSMin) x xs
              in simplifyFieldSize (RangeSize minVal maxVal)
fieldSizeScoped path env (SumField fi) =
  case map (fieldSizeScoped path env . snd) (sumFieldAlternatives fi) of
    [] -> FixedSize 0
    (x:xs) -> let maxVal = foldl' (BinopSize FSMax) x xs
                  minVal = foldl' (BinopSize FSMin) x xs
              in simplifyFieldSize (RangeSize minVal maxVal)
