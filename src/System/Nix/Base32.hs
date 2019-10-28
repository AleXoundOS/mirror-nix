-- |
-- Copyright : © 2018 Shea Levy and 2019 Alexander Tomokhov
-- License   : Apache-2.0

-- Implementation of Nix's base32 encoding.

module System.Nix.Base32
  ( encode, decode
  ) where

import qualified Data.ByteString        as BS
import qualified Data.Text              as T
import qualified Data.Vector            as V
import Data.Bits (shiftL, shiftR, (.|.))
import Data.Word (Word8)

base32chars :: V.Vector Char
base32chars = V.fromList "0123456789abcdfghijklmnpqrsvwxyz" -- omitted: E O U T

-- | Encode a 'BS.ByteString' in Nix's base32 encoding.
encode :: BS.ByteString -> T.Text
encode c = T.pack $ map char32 [nChar - 1, nChar - 2 .. 0]
  where
    -- Each base32 character gives us 5 bits of information, while
    -- each byte gives is 8. Because 'div' rounds down, we need to add
    -- one extra character to the result, and because of that extra 1
    -- we need to subtract one from the number of bits in the
    -- bytestring to cover for the case where the number of bits is
    -- already a factor of 5. Thus, the + 1 outside of the 'div' and
    -- the - 1 inside of it.
    nChar = fromIntegral $ ((BS.length c * 8 - 1) `div` 5) + 1

    byte = BS.index c . fromIntegral

    -- May need to switch to a more efficient calculation at some
    -- point.
    bAsInteger :: Integer
    bAsInteger = sum [fromIntegral (byte j) * (256 ^ j)
                     | j <- [0 .. BS.length c - 1]
                     ]

    char32 :: Integer -> Char
    char32 i = base32chars V.! digitInd
      where
        digitInd = fromIntegral $
                   bAsInteger
                   `div` (32^i)
                   `mod` 32

type Enum32 = Int
newtype Data32 = Data32 {unData32 :: Word}
  deriving (Show)

-- | Decode a 'BS.ByteString' from Nix's base32 encoding.
decode :: T.Text -> BS.ByteString
decode t =
  let dictEnums32 :: V.Vector Enum32
      dictEnums32 = V.map fromEnum base32chars
      dict32ByEnum :: V.Vector (Enum32, Data32)
      dict32ByEnum =
        V.map (\(i, e) -> (e, Data32 $ fromIntegral i)) $ V.indexed dictEnums32
      -- | lookup vector (table)
      dict32ByEnumIndexed :: V.Vector Data32
      dict32ByEnumIndexed =
        V.accumulate
        (flip const) (V.replicate 128 (Data32 undefined)) dict32ByEnum
      -- textEnums32 :: [Enum32]
      -- textEnums32 = map fromEnum $ T.unpack t
      -- data32s :: [Data32]
      -- data32s = map (dict32ByEnumIndexed V.!) textEnums32
      data32s :: V.Vector Data32
      data32s = V.generate
        (T.length t) ((dict32ByEnumIndexed V.!) . fromEnum . T.index t)
  in bitRPack5to8 data32s

-- bitRPack5to8 :: [Data32] -> BS.ByteString
bitRPack5to8 :: V.Vector Data32 -> BS.ByteString
bitRPack5to8 ds =
  -- BS.pack $ foldr processData32 (flip (:) [] . finalize) ds (0x0, initOff)
  BS.pack $ reverse $ V.foldr' processData32 (const []) ds (0x0, initOff)
  where
    initOff = negate $ (5 * V.length ds) `mod` 8
    processData32 ::
      Data32 -> ((Word, Int) -> [Word8]) -> ((Word, Int) -> [Word8])
    processData32 (Data32 d) k (x, s) =
      let x' = x .|. (d `shiftL` (8 + (8 - 5) - s))
          s' = s + 5
      in if s' >= 8
         -- enough bits are accumulated for a complete byte
         then fromIntegral (x' `shiftR` 8)  -- ^ cut the 2nd lowest byte
              : k (x' `shiftL` 8, s' - 8)
         else k (x', s')
    -- finalize (x, _) = fromIntegral $ x `shiftR` 8

-- 1111001110110000111010010100100001111100010110000001100001010110101010101010010010100110010010101110001101100100100111000111111110001001011001101101000101001010110101011001100111110111011010100000000000111001100101000110011110000000000000101100110110111000
-- 1011100011001101000000101000000001100111100101000011100100000000011010101111011110011001110101010100101011010001011001101000100101111111100111000110010011100011010010101010011010100100101010100101011000011000010110000111110001001000111010011011000011110011

-- 01101001100111000001
-- 00001011100011001101
