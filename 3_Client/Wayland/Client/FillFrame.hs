module Wayland.Client.FillFrame
where

import Graphics.Rendering.Cairo
import Foreign (ForeignPtr, withForeignPtr)
import Foreign.C.Types

fillFrame :: ForeignPtr CUChar -> Int -> Int -> IO ()
fillFrame pbuff width heigth = do
    withForeignPtr pbuff $ \p -> do
       surface <- createImageSurfaceForData p FormatARGB32 width heigth (width*4)
       renderWith surface renderImage
       pure ()
  where
    renderImage :: Render ()
    renderImage = do
        selectFontFace "serif" FontSlantNormal  FontWeightBold
        setFontSize 32.0
        setSourceRGB 0.0 0.0 1.0
        moveTo 10.0 50.0
        showText "Hello World"
