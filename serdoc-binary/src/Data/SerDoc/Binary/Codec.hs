{-# LANGUAGE TypeFamilies #-} {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.SerDoc.Binary.Codec
where

import Data.SerDoc.Class
import Data.SerDoc.Info

import qualified Data.Binary as B
import qualified Data.Binary.Put as B
import Data.Word
import Data.Int
import Numeric.Natural
import Data.Proxy
import Data.List
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Short (ShortByteString)

data BinaryCodec

instance Codec BinaryCodec where
  type Context BinaryCodec = ()
  type Encoded BinaryCodec = ()
  type MonadEncode BinaryCodec = B.PutM
  type MonadDecode BinaryCodec = B.Get

instance B.Binary a => Serializable BinaryCodec a where
  encode _ () = B.put
  decodeM _ _ () = B.get

instance HasInfo BinaryCodec () where
  info _ _ = basicField "()" (FixedSize 0)

instance HasInfo BinaryCodec Bool where
  info codec p = enumInfo codec p (Proxy @Word8)

instance HasInfo BinaryCodec Ordering where
  info codec p = enumInfo codec p (Proxy @Word8)

instance HasInfo BinaryCodec Word8 where
  info _ _ = basicField "Word8" (FixedSize 1)

instance HasInfo BinaryCodec Word16 where
  info _ _ = basicField "Word16BE" (FixedSize 2)

instance HasInfo BinaryCodec Word32 where
  info _ _ = basicField "Word32BE" (FixedSize 4)

instance HasInfo BinaryCodec Word64 where
  info _ _ = basicField "Word64BE" (FixedSize 8)

instance HasInfo BinaryCodec Word where
  info _ _ = basicField "Word64BE" (FixedSize 8)

instance HasInfo BinaryCodec Int8 where
  info _ _ = basicField "Int8" (FixedSize 1)

instance HasInfo BinaryCodec Int16 where
  info _ _ = basicField "Int16BE" (FixedSize 2)

instance HasInfo BinaryCodec Int32 where
  info _ _ = basicField "Int32BE" (FixedSize 4)

instance HasInfo BinaryCodec Int64 where
  info _ _ = basicField "Int64BE" (FixedSize 8)

instance HasInfo BinaryCodec Int where
  info _ _ = basicField "Int64BE" (FixedSize 8)

instance HasInfo BinaryCodec Float where
  info _ _ = basicField "Float" UnknownSize

instance HasInfo BinaryCodec Double where
  info _ _ = basicField "Double" UnknownSize

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

instance HasInfo BinaryCodec a => HasInfo BinaryCodec [a] where
  info codec (_ :: Proxy [a]) =
    compoundField "List"
      [ ("length", info codec (Proxy :: Proxy Int))
      , ("items", listField (VarSize "length") (info codec (Proxy :: Proxy a)))
      ]

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

tupleInfo :: [FieldInfo codec] -> FieldInfo codec
tupleInfo fieldInfos =
  compoundField combinedName subfieldInfos
  where
    combinedName = "(" <> (mconcat . intersperse ",") (map shortFieldType fieldInfos) <> ")"
    subfieldInfos =
      [ ("elem" <> show (n :: Int), fi)
      | (n, fi) <- zip [0,1..] fieldInfos
      ]

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

instance HasInfo BinaryCodec LBS.ByteString where
  info codec _ =
    compoundField "List"
      [ ("length", info codec (Proxy :: Proxy Int))
      , ("items", listField (VarSize "length") (info codec (Proxy :: Proxy Word8)))
      ]

instance HasInfo BinaryCodec ShortByteString where
  info codec _ =
    compoundField "List"
      [ ("length", info codec (Proxy :: Proxy Int))
      , ("items", listField (VarSize "length") (info codec (Proxy :: Proxy Word8)))
      ]
