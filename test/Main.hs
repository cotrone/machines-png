{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import           Codec.Picture
import           Codec.Picture.Png.Internal.Type
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.State.Strict (execStateT)
import           Control.Monad.Trans
import qualified Data.Binary as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Bits
import           Data.IntCast
import           Data.Machine
import qualified Data.Machine.Source as Machine
import           Data.Machine.Png
import           Data.Machine.Seekable
import           Data.Proxy
import           Data.Word
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           System.IO
import           System.IO.Temp
import           Test.Tasty
import           Test.Tasty.Hedgehog

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "machines-png"
  [ testGroup "CRC32 Checks"
    [ testProperty "Empty CRC32" (property $ compareCrc [])
    , testProperty "Mempty CRC32" (property $ compareCrc [mempty])
    , checkCrc
    ]
  , testGroup "Imgs Eq"
    [ isEq (Gen.word8 Range.constantBounded) (\case {ImageY8 i -> pure i; _ -> Left "Wrong Image Type"})
    , isEq (PixelYA8 <$> Gen.word8 Range.constantBounded <*> Gen.word8 Range.constantBounded) (\case {ImageYA8 i -> pure i; _ -> Left "Wrong Image Type"})
    , isEq (PixelRGB8 <$> Gen.word8 Range.constantBounded <*> Gen.word8 Range.constantBounded <*> Gen.word8 Range.constantBounded) (\case {ImageRGB8 i -> pure i; _ -> Left "Wrong Image Type"})
    , isEq (PixelRGBA8 <$> Gen.word8 Range.constantBounded <*> Gen.word8 Range.constantBounded <*> Gen.word8 Range.constantBounded <*> Gen.word8 Range.constantBounded) (\case {ImageRGBA8 i -> pure i; _ -> Left "Wrong Image Type"})
    , isEq (Gen.word16 Range.constantBounded) (\case {ImageY16 i -> pure i; _ -> Left "Wrong Image Type"})
    , isEq (PixelYA16 <$> Gen.word16 Range.constantBounded <*> Gen.word16 Range.constantBounded) (\case {ImageYA16 i -> pure i; _ -> Left "Wrong Image Type"})
    , isEq (PixelRGB16 <$> Gen.word16 Range.constantBounded <*> Gen.word16 Range.constantBounded <*> Gen.word16 Range.constantBounded) (\case {ImageRGB16 i -> pure i; _ -> Left "Wrong Image Type"})
    , isEq (PixelRGBA16 <$> Gen.word16 Range.constantBounded <*> Gen.word16 Range.constantBounded <*> Gen.word16 Range.constantBounded <*> Gen.word16 Range.constantBounded) (\case {ImageRGBA16 i -> pure i; _ -> Left "Wrong Image Type"})
    ]
  , testProperty "Large" $ property $ do -- Should make random image, only do once.
      imgBS <- liftIO $ withSystemTempFile "large.png" $ \fn h -> do
        void $ runT $ (Machine.repeated (PixelRGBA8 0 0 0 0) ~> writePNG (Proxy @PNGCompFast) 2048 2048 [] ~> outputHandle h)
        hClose h
        r <- BSL.readFile fn
        pure $!! r
      case decodePng (BSL.toStrict imgBS) of
        Left e -> fail $ "Didn't decode: "<>e
        Right (ImageRGBA8 imgD) ->
          void $ imageIPixels (\(_x, _y, p) -> if p == (PixelRGBA8 0 0 0 0) then pure p else fail "Pixels didn't match!") imgD
        Right _ -> fail "Wrong image pixel type"
  ]

checkCrc :: TestTree
checkCrc = testProperty "Property test CRC" $ property $ do
  bss <- (`replicateM` (forAll $ Gen.bytes (Range.constantFrom 100 0 1000))) =<< (forAll $ Gen.integral (Range.constantFrom 5 0 10))
  compareCrc bss

compareCrc :: [BS.ByteString] -> PropertyT IO ()
compareCrc bss = do
  let actCrc = pngComputeCrc (map BSL.fromStrict bss)
  runT_ $ construct $ do
    (_, (size, crc)) <- (`execStateT` (undefined, (0, 0xFFFFFFFF))) $ do
      forM_ bss (yieldChecksumming . BSL.fromStrict)
    lift $ size === (fromIntegral $ BSL.length $ BSL.fromChunks bss)
    lift $ (0xFFFFFFFF `xor` crc) === actCrc

deriving instance Show PngRawChunk
deriving instance Show PngRawImage

isEq :: forall pixel . (Pixel pixel, PngPixel pixel, Show pixel) => (forall m . MonadGen m => m pixel) -> (DynamicImage -> Either String (Image pixel)) -> TestTree
isEq genPxl unDyn = testProperty "Is Equal" $ property $ do
  w <- forAll $ Gen.element [1..10::Word8]
  h <- forAll $ Gen.element [1..10::Word8]
  imgI <- withImage (intCast w) (intCast h) $ \_ _ -> forAll genPxl
  imgBS <- fmap last . runT $ (construct $ void $ imagePixels (\p -> yield p >> pure p) imgI) ~> writePNG (Proxy @PNGCompFast) (intCast w) (intCast h) [] ~> outputBS
  case fmap unDyn $ decodePng (BSL.toStrict imgBS) of
    Left e -> fail $ "Didn't decode: "<>e
    Right (Left e) -> fail $ "Didn't decode: "<>e
    Right (Right imgD) -> do
      void $ imageIPixels (\(x, y, p) -> if p == pixelAt imgD x y then pure p else fail "Pixels didn't match!") imgI
      void $ imageIPixels (\(x, y, p) -> if p == pixelAt imgI x y then pure p else fail "Pixels didn't match!") imgD
