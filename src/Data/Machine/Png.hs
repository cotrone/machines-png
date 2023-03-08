{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Machine.Png
 ( writePNG, writePNGByLine
 , PNGCompFast(..)
 , Width, Height, ExtraPNGChunks
 , PNGLineCompressor(..), PngPixel(..)
 , yieldChecksumming
 ) where

import qualified Codec.Compression.Zlib.Internal as Z
import           Codec.Picture.Png.Internal.Type
import           Codec.Picture.Types
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.State.Strict (StateT, execStateT)
import qualified Control.Monad.State.Strict as State
import           Control.Monad.Trans
import           Data.Bifunctor
import qualified Data.Binary as B
import qualified Data.Binary.Put as B
import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (foldl')
import           Data.Function (fix)
import           Data.IntCast
import           Data.Kind
import           Data.Machine ((~>))
import qualified Data.Machine as Machine
import qualified Data.Machine.Seekable as Seekable
import           Data.Proxy
import           Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import           Data.Word

class PNGLineCompressor c where
  pngCompLine :: forall p v . (Vector v (PxlUnboxable p), PngPixel p) => Proxy (c, p) -> Maybe (v (PxlUnboxable p)) -> (v (PxlUnboxable p)) -> BSL.ByteString

data PNGCompFast = PNGCompFast

instance PNGLineCompressor PNGCompFast where
  {-# SPECIALIZE INLINE pngCompLine :: Proxy (PNGCompFast, PixelRGBA8) -> Maybe (VU.Vector (PxlUnboxable PixelRGBA8)) -> VU.Vector (PxlUnboxable PixelRGBA8) -> BSL.ByteString #-}
  {-# SPECIALIZE INLINE pngCompLine :: Proxy (PNGCompFast, PixelRGB8) -> Maybe (VU.Vector (PxlUnboxable PixelRGB8)) -> VU.Vector (PxlUnboxable PixelRGB8) -> BSL.ByteString #-}
  {-# SPECIALIZE INLINE pngCompLine :: Proxy (PNGCompFast, Pixel8) -> Maybe (VU.Vector (PxlUnboxable Pixel8)) -> VU.Vector (PxlUnboxable Pixel8) -> BSL.ByteString #-}
  {-# SPECIALIZE INLINE pngCompLine :: Proxy (PNGCompFast, PixelYA8) -> Maybe (VU.Vector (PxlUnboxable PixelYA8)) -> VU.Vector (PxlUnboxable PixelYA8) -> BSL.ByteString #-}
  {-# SPECIALIZE INLINE pngCompLine :: Proxy (PNGCompFast, PixelYA16) -> Maybe (VU.Vector (PxlUnboxable PixelYA16)) -> VU.Vector (PxlUnboxable PixelYA16) -> BSL.ByteString #-}
  {-# SPECIALIZE INLINE pngCompLine :: Proxy (PNGCompFast, Pixel16) -> Maybe (VU.Vector (PxlUnboxable Pixel16)) -> VU.Vector (PxlUnboxable Pixel16) -> BSL.ByteString #-}
  {-# SPECIALIZE INLINE pngCompLine :: Proxy (PNGCompFast, PixelRGB16) -> Maybe (VU.Vector (PxlUnboxable PixelRGB16)) -> VU.Vector (PxlUnboxable PixelRGB16) -> BSL.ByteString #-}
  {-# SPECIALIZE INLINE pngCompLine :: Proxy (PNGCompFast, PixelRGBA16) -> Maybe (VU.Vector (PxlUnboxable PixelRGBA16)) -> VU.Vector (PxlUnboxable PixelRGBA16) -> BSL.ByteString #-}
  pngCompLine :: forall c v p . (Vector v (PxlUnboxable p), PngPixel p) => Proxy (c, p) -> Maybe (v (PxlUnboxable p)) -> (v (PxlUnboxable p)) -> BSL.ByteString
  pngCompLine _ _ v = B.runPut $ do
    B.put (0::Word8) -- No filter
    VG.mapM_ (pxEncode . (pxlFromUnbox @p)) v

type Width = Word32
type Height = Word32

-- | Must not contain Critical Chunks (IHDR, PLTE, IDAT, IEND)
type ExtraPNGChunks = [PngRawChunk]

writePNG :: forall c p m i . (PngPixel p, VU.Unbox (PxlUnboxable p), PrimMonad m, PNGLineCompressor c) => Proxy c -> Width -> Height -> ExtraPNGChunks -> Machine.ProcessT m p (Seekable.Sought i ByteString)
writePNG _ w h ext = (Machine.construct $ getPxLine h) ~> writePNGByLine (Proxy @(c, p)) w h ext
  where
    getPxLine 0 = pure ()
    getPxLine n = do
      Machine.yield =<< VU.replicateM (intCast w) (pxlToUnbox <$> Machine.await)
      getPxLine (n-1)
{-# INLINABLE writePNG #-}

yieldChunks :: BSL.ByteString -> Machine.Plan k (Seekable.Sought i ByteString) ()
yieldChunks = mapM_ (Machine.yield . Seekable.Write) . BSL.toChunks

yieldPut :: B.Put -> Machine.Plan k (Seekable.Sought i ByteString) ()
yieldPut = yieldChunks . B.runPut

type CRC32 = Word32


-- | From the Annex D of the png specification.
pngCrcTable :: VU.Vector Word32
pngCrcTable = VU.fromListN 256 [ foldl' updateCrcConstant c [zero .. 7] | c <- [0 .. 255] ]
    where zero = 0 :: Int
          updateCrcConstant c _ | c .&. 1 /= 0 = magicConstant `xor` (c `unsafeShiftR` 1)
                                | otherwise = c `unsafeShiftR` 1
          magicConstant = 0xedb88320 :: Word32

yieldChecksumming :: forall m k i a . (Monad m) => BSL.ByteString -> StateT (a, (Word32, CRC32)) (Machine.PlanT k (Seekable.Sought i  ByteString) m) ()
yieldChecksumming =
    mapM_ (\b -> State.modify' (fmap $ bimap ((fromIntegral $ BS.length b)+) (\c -> BS.foldl' updateCrc c b)) >> lift (Machine.yield (Seekable.Write b))) . BSL.toChunks
  where
    updateCrc crc val =
      let u32Val = fromIntegral val
          lutVal = pngCrcTable VU.! (fromIntegral ((crc `xor` u32Val) .&. 0xFF))
       in lutVal `xor` (crc `unsafeShiftR` 8)

yieldCompressing :: forall m k i mb. (Monad m, PrimMonad m, PrimBase mb, PrimState m ~ PrimState mb) => BSL.ByteString -> StateT (Z.CompressStream mb, (Word32, CRC32)) (Machine.PlanT k (Seekable.Sought i  ByteString) m) ()
yieldCompressing = mapM_ emit . BSL.toChunks
  where
    emit :: ByteString -> StateT (Z.CompressStream mb, (Word32, CRC32)) (Machine.PlanT k (Seekable.Sought i ByteString) m) ()
    emit b = do
      c <- fst <$> State.get
      case c of
        Z.CompressInputRequired spl -> (lift $ lift $ primToPrim $ spl b) >>= State.modify' . first . const
        Z.CompressOutputAvailable out nxt -> yieldChecksumming (BSL.fromStrict out) >> (lift $ lift $ primToPrim nxt) >>= State.modify' . first . const >> emit b
        Z.CompressStreamEnd -> fail "Zlib ended before finished!"

writePNGByLine :: forall c p m i . (PngPixel p, VU.Unbox (PxlUnboxable p), PrimMonad m, PNGLineCompressor c) => Proxy (c, p) -> Width -> Height -> ExtraPNGChunks -> Machine.ProcessT m (VU.Vector (PxlUnboxable p)) (Seekable.Sought i ByteString)
writePNGByLine p w h extras = Machine.construct $ do
  unless (h > 0) $ fail "PNG must contain at least a single pixel"
  yieldChunks pngSignature
  yieldChunks $ B.encode $ PngIHdr
           { width             = w
           , height            = h
           , bitDepth          = pngDepth (Proxy @p)
           , colourType        = pngType (Proxy @p)
           , compressionMethod = 0
           , filterMethod      = 0
           , interlaceMethod   = PngNoInterlace
           }
  forM_ extras $ yieldChunks . B.encode 
  -- Make iDAT chunk
  Machine.yield Seekable.SoughtMark -- We'll want to come back here to write the size of this chunk when we're done.
  yieldPut $ B.putWord32be (0::Word32)
  (_, (size, crc)) <- (`execStateT` (Z.compressST Z.zlibFormat (Z.defaultCompressParams {Z.compressStrategy = Z.filteredStrategy}), (0, 0xFFFFFFFF))) $ do
    yieldChecksumming $ iDATSignature
    l <- lift Machine.await
    yieldCompressing $ pngCompLine p Nothing l
    iterateMN_ (\o -> lift Machine.await >>= \n -> yieldCompressing (pngCompLine p (Just o) n) >> pure n) (h-1) l
    -- Time to force the output of the compression stream
    fix (\cont -> \case
       Z.CompressInputRequired spl -> (lift $ lift $ primToPrim $ spl mempty) >>= cont
       Z.CompressOutputAvailable out nxt -> yieldChecksumming (BSL.fromStrict out) >> (lift $ lift $ primToPrim nxt) >>= cont
       Z.CompressStreamEnd -> pure ()
       ) =<< fmap fst State.get
  yieldPut $ B.putWord32be $ 0xFFFFFFFF `xor` crc
  Machine.yield Seekable.SoughtRet -- Return and write the size
  yieldPut $ B.putWord32be $ (size-4) -- We need to remove the size of the signature.
  Machine.yield Seekable.SoughtEnd -- Return so we can write the remaining chunk
  -- Finished iDAT chunk
  yieldChunks $ B.encode $ mkRawChunk iENDSignature mempty
{-# INLINABLE writePNGByLine #-}

iterateMN_ :: Monad m => (a -> m a) -> Word32 -> a -> m ()
iterateMN_ _   0 _ = pure ()
iterateMN_ act n a = act a >>= iterateMN_ act (n-1)

class (VU.Unbox (PxlUnboxable p)) => PngPixel p where
  type PxlUnboxable p :: Type
  pngType :: Proxy p -> PngImageType
  pngDepth :: Proxy p -> Word8
  pxEncode :: p -> B.Put
  pxlToUnbox :: p -> PxlUnboxable p
  pxlFromUnbox :: PxlUnboxable p -> p

instance PngPixel PixelRGBA8 where
  type PxlUnboxable PixelRGBA8 = (Word8, Word8, Word8, Word8)
  pngType  _ = PngTrueColourWithAlpha
  pngDepth _ = 8
  pxEncode (PixelRGBA8 r g b a) = B.putWord8 r >> B.putWord8 g >> B.putWord8 b >> B.putWord8 a
  {-# INLINE pxEncode #-}
  pxlToUnbox (PixelRGBA8 r g b a) = (r, g, b, a)
  {-# INLINE pxlToUnbox #-}
  pxlFromUnbox (r, g, b, a) = (PixelRGBA8 r g b a)
  {-# INLINE pxlFromUnbox #-}

instance PngPixel PixelRGB8 where
  type PxlUnboxable PixelRGB8 = (Word8, Word8, Word8)
  pngType  _ = PngTrueColour
  pngDepth _ = 8
  pxEncode (PixelRGB8 r g b) = B.putWord8 r >> B.putWord8 g >> B.putWord8 b
  {-# INLINE pxEncode #-}
  pxlToUnbox (PixelRGB8 r g b) = (r, g, b)
  {-# INLINE pxlToUnbox #-}
  pxlFromUnbox (r, g, b) = (PixelRGB8 r g b)
  {-# INLINE pxlFromUnbox #-}

instance PngPixel Pixel8 where
  type PxlUnboxable Pixel8 = Word8
  pngType  _ = PngGreyscale
  pngDepth _ = 8
  pxEncode  = B.putWord8
  {-# INLINE pxEncode #-}
  pxlToUnbox = id
  {-# INLINE pxlToUnbox #-}
  pxlFromUnbox = id
  {-# INLINE pxlFromUnbox #-}

instance PngPixel PixelYA8 where
  type PxlUnboxable PixelYA8 = (Word8, Word8)
  pngType  _ = PngGreyscaleWithAlpha
  pngDepth _ = 8
  pxEncode (PixelYA8 l a) = B.putWord8 l >> B.putWord8 a
  {-# INLINE pxEncode #-}
  pxlToUnbox (PixelYA8 l a) = (l, a)
  {-# INLINE pxlToUnbox #-}
  pxlFromUnbox (l, a) = PixelYA8 l a
  {-# INLINE pxlFromUnbox #-}

instance PngPixel PixelYA16 where
  type PxlUnboxable PixelYA16 = (Word16, Word16)
  pngType  _ = PngGreyscaleWithAlpha
  pngDepth _ = 16
  pxEncode (PixelYA16 l a) = B.putWord16be l >> B.putWord16be a
  {-# INLINE pxEncode #-}
  pxlToUnbox (PixelYA16 l a) = (l, a)
  {-# INLINE pxlToUnbox #-}
  pxlFromUnbox (l, a) = (PixelYA16 l a)
  {-# INLINE pxlFromUnbox #-}

instance PngPixel Pixel16 where
  type PxlUnboxable Pixel16 = Word16
  pngType  _ = PngGreyscale
  pngDepth _ = 16
  pxEncode   = B.putWord16be
  {-# INLINE pxEncode #-}
  pxlToUnbox = id
  {-# INLINE pxlToUnbox #-}
  pxlFromUnbox = id
  {-# INLINE pxlFromUnbox #-}

instance PngPixel PixelRGBA16 where
  type PxlUnboxable PixelRGBA16 = (Word16, Word16, Word16, Word16)
  pngType  _ = PngTrueColourWithAlpha
  pngDepth _ = 16
  pxEncode (PixelRGBA16 r g b a) = B.putWord16be r >> B.putWord16be g >> B.putWord16be b >> B.putWord16be a
  {-# INLINE pxEncode #-}
  pxlToUnbox (PixelRGBA16 r g b a) = (r, g, b, a)
  {-# INLINE pxlToUnbox #-}
  pxlFromUnbox (r, g, b, a) = (PixelRGBA16 r g b a)
  {-# INLINE pxlFromUnbox #-}

instance PngPixel PixelRGB16 where
  type PxlUnboxable PixelRGB16 = (Word16, Word16, Word16)
  pngType  _ = PngTrueColour
  pngDepth _ = 16
  pxEncode (PixelRGB16 r g b) = B.putWord16be r >> B.putWord16be g >> B.putWord16be b
  {-# INLINE pxEncode #-}
  pxlToUnbox (PixelRGB16 r g b) = (r, g, b)
  {-# INLINE pxlToUnbox #-}
  pxlFromUnbox (r, g, b) = PixelRGB16 r g b
  {-# INLINE pxlFromUnbox #-}
