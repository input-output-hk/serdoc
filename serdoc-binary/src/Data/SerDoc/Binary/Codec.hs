{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.SerDoc.Binary.Codec
where

import Data.SerDoc.Class as SerDoc
import Data.SerDoc.Info

import qualified Data.Binary as B
import qualified Data.Binary.Put as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Short (ShortByteString)
import Data.Int
import Data.List
import Data.Proxy
import Data.Word
import Numeric.Natural

data BinaryCodec

instance Codec BinaryCodec where
  type MonadEncode BinaryCodec = B.PutM
  type MonadDecode BinaryCodec = B.Get

newtype ViaBinary a = ViaBinary { viaBinary :: a }

instance B.Binary a => Serializable BinaryCodec (ViaBinary a) where
  encode _ = B.put . viaBinary
  decode _ = ViaBinary <$> B.get

instance HasInfo BinaryCodec () where
  info _ _ = basicField "()" (FixedSize 0)

deriving via (ViaBinary ()) instance Serializable BinaryCodec ()

instance HasInfo BinaryCodec Bool where
  info codec p = enumInfo codec p (Proxy @Word8)

deriving via (ViaBinary Bool) instance Serializable BinaryCodec Bool

instance HasInfo BinaryCodec Ordering where
  info codec p = enumInfo codec p (Proxy @Word8)

deriving via (ViaBinary Ordering) instance Serializable BinaryCodec Ordering

instance HasInfo BinaryCodec Word8 where
  info _ _ = basicField "Word8" (FixedSize 1)

deriving via (ViaBinary Word8) instance Serializable BinaryCodec Word8

instance HasInfo BinaryCodec Word16 where
  info _ _ = basicField "Word16BE" (FixedSize 2)

deriving via (ViaBinary Word16) instance Serializable BinaryCodec Word16

instance HasInfo BinaryCodec Word32 where
  info _ _ = basicField "Word32BE" (FixedSize 4)

deriving via (ViaBinary Word32) instance Serializable BinaryCodec Word32

instance HasInfo BinaryCodec Word64 where
  info _ _ = basicField "Word64BE" (FixedSize 8)

deriving via (ViaBinary Word64) instance Serializable BinaryCodec Word64

instance HasInfo BinaryCodec Word where
  info _ _ = basicField "Word64BE" (FixedSize 8)

deriving via (ViaBinary Word) instance Serializable BinaryCodec Word

instance HasInfo BinaryCodec Int8 where
  info _ _ = basicField "Int8" (FixedSize 1)

deriving via (ViaBinary Int8) instance Serializable BinaryCodec Int8

instance HasInfo BinaryCodec Int16 where
  info _ _ = basicField "Int16BE" (FixedSize 2)

deriving via (ViaBinary Int16) instance Serializable BinaryCodec Int16

instance HasInfo BinaryCodec Int32 where
  info _ _ = basicField "Int32BE" (FixedSize 4)

deriving via (ViaBinary Int32) instance Serializable BinaryCodec Int32

instance HasInfo BinaryCodec Int64 where
  info _ _ = basicField "Int64BE" (FixedSize 8)

deriving via (ViaBinary Int64) instance Serializable BinaryCodec Int64

instance HasInfo BinaryCodec Int where
  info _ _ = basicField "Int64BE" (FixedSize 8)

deriving via (ViaBinary Int) instance Serializable BinaryCodec Int

instance HasInfo BinaryCodec Float where
  info _ _ = basicField "Float" UnknownSize

deriving via (ViaBinary Float) instance Serializable BinaryCodec Float

instance HasInfo BinaryCodec Double where
  info _ _ = basicField "Double" UnknownSize

deriving via (ViaBinary Double) instance Serializable BinaryCodec Double

instance HasInfo BinaryCodec Integer where
  info codec _ =
    compoundField "Integer"
      [ ("big", info codec (Proxy :: Proxy Bool))
      , ("data",
          choiceField
            (IndexField "big")
            [ info codec (Proxy :: Proxy Int32)
            , compoundField "BigInteger"
                [ ("sign", info codec (Proxy :: Proxy Word8))
                , ("dataBytes", info codec (Proxy :: Proxy [Word8]))
                ]
            ]
        )
      ]

deriving via (ViaBinary Integer) instance Serializable BinaryCodec Integer

instance HasInfo BinaryCodec Natural where
  info codec _ =
    compoundField "Natural"
      [ ("big", info codec (Proxy :: Proxy Bool))
      , ("data",
          choiceField
            (IndexField "big")
            [ info codec (Proxy :: Proxy Word64)
            , info codec (Proxy :: Proxy [Word8])
            ]
        )
      ]

deriving via (ViaBinary Natural) instance Serializable BinaryCodec Natural

instance HasInfo BinaryCodec a => HasInfo BinaryCodec [a] where
  info codec (_ :: Proxy [a]) =
    compoundField "List"
      [ ("length", info codec (Proxy :: Proxy Int))
      , ("items", listField (VarSize "length") (info codec (Proxy :: Proxy a)))
      ]
deriving via (ViaBinary [a]) instance B.Binary a => Serializable BinaryCodec [a]

instance HasInfo BinaryCodec a => HasInfo BinaryCodec (Maybe a) where
  info codec (_ :: Proxy (Maybe a)) =
    compoundField "Maybe"
      [ ("isJust", info codec (Proxy :: Proxy Word8))
      , ("value",
            choiceField
              (IndexField "isJust")
              [ AnnField "Nothing" $ info codec (Proxy :: Proxy ())
              , AnnField "Just" $ info codec (Proxy :: Proxy a)
              ]
        )
      ]
deriving via (ViaBinary (Maybe a)) instance B.Binary a => Serializable BinaryCodec (Maybe a)

instance (HasInfo BinaryCodec a, HasInfo BinaryCodec b)
         => HasInfo BinaryCodec (Either a b)
         where
  info codec (_ :: Proxy (Either a b)) =
    compoundField "Either"
      [ ("constructor", info codec (Proxy :: Proxy Word8))
      , ("value",
            choiceField
              (IndexField "constructor")
              [ AnnField "Left" $ info codec (Proxy :: Proxy a)
              , AnnField "Right" $ info codec (Proxy :: Proxy b)
              ]
        )
      ]

deriving via (ViaBinary (Either a b)) instance (B.Binary a, B.Binary b)
         => Serializable BinaryCodec (Either a b)

tupleInfo :: [FieldInfo codec] -> FieldInfo codec
tupleInfo fieldInfos =
  compoundField combinedName subfieldInfos
  where
    combinedName = "(" <> (mconcat . intersperse ",") (map shortFieldType fieldInfos) <> ")"
    subfieldInfos =
      [ ("elem" <> show (n :: Int), fi)
      | (n, fi) <- zip [0,1..] fieldInfos
      ]

deriving via (ViaBinary (a, b))
  instance
    ( B.Binary a
    , B.Binary b
    )
    =>
    Serializable BinaryCodec (a, b)

deriving via (ViaBinary (a, b, c))
  instance
    ( B.Binary a
    , B.Binary b
    , B.Binary c
    )
    =>
    Serializable BinaryCodec (a, b, c)

deriving via (ViaBinary (a, b, c, d))
  instance
    ( B.Binary a
    , B.Binary b
    , B.Binary c
    , B.Binary d
    )
    =>
    Serializable BinaryCodec (a, b, c, d)

deriving via (ViaBinary (a, b, c, d, e))
  instance
    ( B.Binary a
    , B.Binary b
    , B.Binary c
    , B.Binary d
    , B.Binary e
    )
    =>
    Serializable BinaryCodec (a, b, c, d, e)

deriving via (ViaBinary (a, b, c, d, e, f))
  instance
    ( B.Binary a
    , B.Binary b
    , B.Binary c
    , B.Binary d
    , B.Binary e
    , B.Binary f
    )
    =>
    Serializable BinaryCodec (a, b, c, d, e, f)

deriving via (ViaBinary (a, b, c, d, e, f, g))
  instance
    ( B.Binary a
    , B.Binary b
    , B.Binary c
    , B.Binary d
    , B.Binary e
    , B.Binary f
    , B.Binary g
    )
    =>
    Serializable BinaryCodec (a, b, c, d, e, f, g)

deriving via (ViaBinary (a, b, c, d, e, f, g, h))
  instance
    ( B.Binary a
    , B.Binary b
    , B.Binary c
    , B.Binary d
    , B.Binary e
    , B.Binary f
    , B.Binary g
    , B.Binary h
    )
    =>
    Serializable BinaryCodec (a, b, c, d, e, f, g, h)

deriving via (ViaBinary (a, b, c, d, e, f, g, h, i))
  instance
    ( B.Binary a
    , B.Binary b
    , B.Binary c
    , B.Binary d
    , B.Binary e
    , B.Binary f
    , B.Binary g
    , B.Binary h
    , B.Binary i
    )
    =>
    Serializable BinaryCodec (a, b, c, d, e, f, g, h, i)

deriving via (ViaBinary (a, b, c, d, e, f, g, h, i, j))
  instance
    ( B.Binary a
    , B.Binary b
    , B.Binary c
    , B.Binary d
    , B.Binary e
    , B.Binary f
    , B.Binary g
    , B.Binary h
    , B.Binary i
    , B.Binary j
    )
    =>
    Serializable BinaryCodec (a, b, c, d, e, f, g, h, i, j)

instance ( HasInfo BinaryCodec a
         , HasInfo BinaryCodec b
         )
         => HasInfo BinaryCodec (a, b)
         where
  info codec (_ :: Proxy (a, b)) =
    tupleInfo
      [ info codec (Proxy :: Proxy a)
      , info codec (Proxy :: Proxy b)
      ]

instance ( HasInfo BinaryCodec a
         , HasInfo BinaryCodec b
         , HasInfo BinaryCodec c
         )
         => HasInfo BinaryCodec (a, b, c)
         where
  info codec (_ :: Proxy (a, b, c)) =
    tupleInfo
      [ info codec (Proxy :: Proxy a)
      , info codec (Proxy :: Proxy b)
      , info codec (Proxy :: Proxy c)
      ]

instance ( HasInfo BinaryCodec a
         , HasInfo BinaryCodec b
         , HasInfo BinaryCodec c
         , HasInfo BinaryCodec d
         )
         => HasInfo BinaryCodec (a, b, c, d)
         where
  info codec (_ :: Proxy (a, b, c, d)) =
    tupleInfo
      [ info codec (Proxy :: Proxy a)
      , info codec (Proxy :: Proxy b)
      , info codec (Proxy :: Proxy c)
      , info codec (Proxy :: Proxy d)
      ]

instance ( HasInfo BinaryCodec a
         , HasInfo BinaryCodec b
         , HasInfo BinaryCodec c
         , HasInfo BinaryCodec d
         , HasInfo BinaryCodec e
         )
         => HasInfo BinaryCodec (a, b, c, d, e)
         where
  info codec (_ :: Proxy (a, b, c, d, e)) =
    tupleInfo
      [ info codec (Proxy :: Proxy a)
      , info codec (Proxy :: Proxy b)
      , info codec (Proxy :: Proxy c)
      , info codec (Proxy :: Proxy d)
      , info codec (Proxy :: Proxy e)
      ]

instance ( HasInfo BinaryCodec a
         , HasInfo BinaryCodec b
         , HasInfo BinaryCodec c
         , HasInfo BinaryCodec d
         , HasInfo BinaryCodec e
         , HasInfo BinaryCodec f
         )
         => HasInfo BinaryCodec (a, b, c, d, e, f)
         where
  info codec (_ :: Proxy (a, b, c, d, e, f)) =
    tupleInfo
      [ info codec (Proxy :: Proxy a)
      , info codec (Proxy :: Proxy b)
      , info codec (Proxy :: Proxy c)
      , info codec (Proxy :: Proxy d)
      , info codec (Proxy :: Proxy e)
      , info codec (Proxy :: Proxy f)
      ]

instance ( HasInfo BinaryCodec a
         , HasInfo BinaryCodec b
         , HasInfo BinaryCodec c
         , HasInfo BinaryCodec d
         , HasInfo BinaryCodec e
         , HasInfo BinaryCodec f
         , HasInfo BinaryCodec g
         )
         => HasInfo BinaryCodec (a, b, c, d, e, f, g)
         where
  info codec (_ :: Proxy (a, b, c, d, e, f, g)) =
    tupleInfo
      [ info codec (Proxy :: Proxy a)
      , info codec (Proxy :: Proxy b)
      , info codec (Proxy :: Proxy c)
      , info codec (Proxy :: Proxy d)
      , info codec (Proxy :: Proxy e)
      , info codec (Proxy :: Proxy f)
      , info codec (Proxy :: Proxy g)
      ]

instance ( HasInfo BinaryCodec a
         , HasInfo BinaryCodec b
         , HasInfo BinaryCodec c
         , HasInfo BinaryCodec d
         , HasInfo BinaryCodec e
         , HasInfo BinaryCodec f
         , HasInfo BinaryCodec g
         , HasInfo BinaryCodec h
         )
         => HasInfo BinaryCodec (a, b, c, d, e, f, g, h)
         where
  info codec (_ :: Proxy (a, b, c, d, e, f, g, h)) =
    tupleInfo
      [ info codec (Proxy :: Proxy a)
      , info codec (Proxy :: Proxy b)
      , info codec (Proxy :: Proxy c)
      , info codec (Proxy :: Proxy d)
      , info codec (Proxy :: Proxy e)
      , info codec (Proxy :: Proxy f)
      , info codec (Proxy :: Proxy g)
      , info codec (Proxy :: Proxy h)
      ]


instance ( HasInfo BinaryCodec a
         , HasInfo BinaryCodec b
         , HasInfo BinaryCodec c
         , HasInfo BinaryCodec d
         , HasInfo BinaryCodec e
         , HasInfo BinaryCodec f
         , HasInfo BinaryCodec g
         , HasInfo BinaryCodec h
         , HasInfo BinaryCodec i
         )
         => HasInfo BinaryCodec (a, b, c, d, e, f, g, h, i)
         where
  info codec (_ :: Proxy (a, b, c, d, e, f, g, h, i)) =
    tupleInfo
      [ info codec (Proxy :: Proxy a)
      , info codec (Proxy :: Proxy b)
      , info codec (Proxy :: Proxy c)
      , info codec (Proxy :: Proxy d)
      , info codec (Proxy :: Proxy e)
      , info codec (Proxy :: Proxy f)
      , info codec (Proxy :: Proxy g)
      , info codec (Proxy :: Proxy h)
      , info codec (Proxy :: Proxy i)
      ]

instance ( HasInfo BinaryCodec a
         , HasInfo BinaryCodec b
         , HasInfo BinaryCodec c
         , HasInfo BinaryCodec d
         , HasInfo BinaryCodec e
         , HasInfo BinaryCodec f
         , HasInfo BinaryCodec g
         , HasInfo BinaryCodec h
         , HasInfo BinaryCodec i
         , HasInfo BinaryCodec j
         )
         => HasInfo BinaryCodec (a, b, c, d, e, f, g, h, i, j)
         where
  info codec (_ :: Proxy (a, b, c, d, e, f, g, h, i, j)) =
    tupleInfo
      [ info codec (Proxy :: Proxy a)
      , info codec (Proxy :: Proxy b)
      , info codec (Proxy :: Proxy c)
      , info codec (Proxy :: Proxy d)
      , info codec (Proxy :: Proxy e)
      , info codec (Proxy :: Proxy f)
      , info codec (Proxy :: Proxy g)
      , info codec (Proxy :: Proxy h)
      , info codec (Proxy :: Proxy i)
      , info codec (Proxy :: Proxy j)
      ]

instance HasInfo BinaryCodec ByteString where
  info codec _ =
    compoundField "List"
      [ ("length", info codec (Proxy :: Proxy Int))
      , ("items", listField (VarSize "length") (info codec (Proxy :: Proxy Word8)))
      ]
deriving via (ViaBinary ByteString) instance Serializable BinaryCodec ByteString

instance HasInfo BinaryCodec LBS.ByteString where
  info codec _ =
    compoundField "List"
      [ ("length", info codec (Proxy :: Proxy Int))
      , ("items", listField (VarSize "length") (info codec (Proxy :: Proxy Word8)))
      ]
deriving via (ViaBinary LBS.ByteString) instance Serializable BinaryCodec LBS.ByteString

instance HasInfo BinaryCodec ShortByteString where
  info codec _ =
    compoundField "List"
      [ ("length", info codec (Proxy :: Proxy Int))
      , ("items", listField (VarSize "length") (info codec (Proxy :: Proxy Word8)))
      ]
deriving via (ViaBinary ShortByteString) instance Serializable BinaryCodec ShortByteString
