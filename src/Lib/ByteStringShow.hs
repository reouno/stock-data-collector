{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.ByteStringShow where

import           Data.ByteString    ( ByteString, intercalate )
import           Data.Text.Encoding ( encodeUtf8 )
import           TextShow           ( showt )

class ByteStringShow a where
  showB :: a -> ByteString

instance ByteStringShow ByteString where
  showB = id

instance ByteStringShow Int where
  showB = encodeUtf8 . showt

instance ByteStringShow [ByteString] where
  showB = intercalate ","
