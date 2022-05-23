{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Louis
-- Copyright   : (c) Alexey Kutepov 2019
-- License     : MIT
-- Maintainer  : tsodingbiz@gmail.com
-- Portability : portable
--
-- >>> import Louis
-- >>> import qualified Data.Text as T
-- >>> putStrLn . T.unpack . T.unlines =<< braillizeFile "image.png"
-- ⠀⠀⠀⡸⠿⠿⠿⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
-- ⠀⢀⣴⣶⣶⣶⣶⣶⣶⣦⣬⣉⠻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
-- ⠀⣸⣿⣿⣿⣿⣿⣿⣿⡿⢿⣿⡆⣿⣿⣿⣿⣿⣿⣿⣿⡿⠿⠿⠿⢿⣿⣿⣿⣿
-- ⠀⣿⣿⣿⣿⣿⣿⣿⣿⠁⢠⣿⠡⠿⠿⠿⠿⣿⣿⣿⢃⣶⣾⣿⣷⣶⣶⣤⣍⡛
-- ⡀⢻⣿⣿⣿⣿⣿⣿⣿⣤⣾⠋⠐⠲⠶⣦⣤⣌⡙⠋⢸⣿⠋⠙⣿⣿⣿⣿⣿⣿
-- ⣿⣦⣭⣉⣉⣙⣛⣋⣉⣉⣅⣾⣿⣿⣷⣾⣿⣿⣿⡇⣿⡏⠀⢰⣿⣿⣿⣿⣿⣿
-- ⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣇⢹⣿⣾⣿⣿⣿⣿⣿⣿⣿
-- ⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣶⣬⡙⠛⢿⣿⣿⣿⣿⣿
-- ⣿⣿⣿⣿⣿⣿⣿⣿⣿⡝⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣶⣬⣍⣛⠻⠿
-- ⣿⣿⣿⣿⣿⣿⣿⣿⣿⠃⣶⣶⣶⣦⡉⣰⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
-- ⣿⣿⣿⣿⣿⣿⣿⣿⣯⣾⣿⣿⣿⣿⣇⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
-- ⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
-- ⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
-- ⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
-- ⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿

module Louis
  ( braillizeDynamicImage
  , braillizeByteString
  , braillizeFile
  , resizeImageWidth
  ) where

import Data.Word
import Data.Char
import Data.Bits
import Codec.Picture
import qualified Data.Vector.Storable as V
import Data.List
import qualified Data.Text as T
import Data.Functor.Compose
import qualified Data.ByteString as BS

type Chunk = Word8

renderChunk :: Chunk -> Char
renderChunk x = chr (bgroup * groupSize + boffset + ord '⠀')
  where
    bgroup =
      let b1 = (x .&. 0b00001000) `shiftR` 3
          b2 = (x .&. 0b10000000) `shiftR` 6
       in fromIntegral (b1 .|. b2)
    boffset =
      let b1 = (x .&. 0b00000111)
          b2 = (x .&. 0b01110000) `shiftR` 1
       in fromIntegral (b1 .|. b2)
    groupSize = 64

chunkifyGreyScale :: Image Pixel8 -> [[Chunk]]
chunkifyGreyScale =
  let squashBits :: [Word8] -> Word8
      squashBits = foldl' (\acc x -> shiftL acc 1 .|. x) 0
      k :: Image Pixel8 -> Word8 -> Pixel8 -> Word8
      k img threshold x
        | x < threshold = 0
        | otherwise = 1
      f :: Image Pixel8 -> Word8 -> (Int, Int) -> Word8
      f img threshold (x, y)
        | 0 <= x && x < (imageWidth img) && 0 <= y && y < (imageHeight img) =
          k img threshold $ pixelAt img x y
        | otherwise = 0
      chunkAt :: Image Pixel8 -> Word8 -> (Int, Int) -> Chunk
      chunkAt img threshold (x, y) =
        squashBits $ reverse [f img threshold (i + x, j + y) | i <- [0, 1], j <- [0 .. 3]]
   in \img ->
    let width = imageWidth img
        height = imageHeight img
        chunksWidth = width `div` 2
        chunksHeight = height `div` 4
        threshold =
          let imgData = imageData img
          in round
             $ fromIntegral
             (V.foldl' (flip (.) fromIntegral . (+)) (0 :: Word32) imgData)
             / (fromIntegral $ V.length imgData)
    in [ [chunkAt img threshold (i * 2, j * 4)
         | i <- [0 .. chunksWidth - 1]
         ]
       | j <- [0 .. chunksHeight - 1]
       ]

greyScaleImage :: Image PixelRGBA8 -> Image Pixel8
greyScaleImage = pixelMap greyScalePixel
  -- reference: https://www.mathworks.com/help/matlab/ref/rgb2gray.html
  where
    greyScalePixel :: PixelRGBA8 -> Pixel8
    greyScalePixel (PixelRGBA8 r g b a) = round
      ((fromIntegral r * 0.299
        + fromIntegral g * 0.587
        + fromIntegral b * 0.114)
       * fromIntegral a / 255.0)

braillizeGreyScale :: Image Pixel8 -> [T.Text]
braillizeGreyScale =
  map T.pack . getCompose . fmap renderChunk . Compose . chunkifyGreyScale

resizeImageWidth :: Pixel a => Int -> Image a -> Image a
resizeImageWidth width' image'
  | width /= width' =
    let ratio :: Float
        ratio = fromIntegral width' / fromIntegral width
        height' = floor (fromIntegral height * ratio)
        y_interval :: Float
        y_interval = fromIntegral height / fromIntegral height'
        x_interval :: Float
        x_interval = fromIntegral width / fromIntegral width'
        resizedImage = generateImage
          (\x y -> pixelAt image'
                   (floor $ fromIntegral x * x_interval)
                   (floor $ fromIntegral y * y_interval))
          width'
          height'
     in resizedImage
  | otherwise = image'
  where
    width = imageWidth image'
    height = imageHeight image'

braillizeDynamicImage :: Int -> DynamicImage -> [T.Text]
braillizeDynamicImage width = braillizeGreyScale . greyScaleImage . resizeImageWidth width . convertRGBA8

braillizeByteString :: Int -> BS.ByteString -> Either String [T.Text]
braillizeByteString width bytes = braillizeDynamicImage width <$> decodeImage bytes

braillizeFile :: Int -> FilePath -> IO [T.Text]
braillizeFile width filePath = do
  bytes <- BS.readFile filePath
  either error return $ braillizeByteString width bytes
