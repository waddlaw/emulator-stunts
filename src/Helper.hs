{-# OPTIONS_GHC -fno-warn-orphans #-}

module Helper where

import Data.Bits
import Data.Word
import Numeric

instance Num Bool where
  (+) = xor
  (-) = xor
  (*) = (&&)
  abs = id
  signum = id
  fromInteger = odd

instance Real Bool where
  toRational = toRational . fromEnum

instance Integral Bool where
  toInteger = toInteger . fromEnum
  a `quotRem` 1 = (a, 0)
  a `quotRem` _ = error $ "quotRem " ++ show a ++ " 0 :: Bool"

----------------------------------------------

{-# INLINE debug #-}
debug :: Bool
debug = False

showHex' :: (Show a, Integral a) => Int -> a -> String
showHex' i x = replicate (i - length s) '0' ++ s where s = showHex x ""

segAddr :: Word16 -> Word16 -> Int
segAddr s w = fromIntegral s `shiftL` 4 + fromIntegral w
