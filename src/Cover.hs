-- Copyright © 2012 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

module Cover
where

import qualified Data.ByteString.Lazy as BS
import Data.Int
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango
import System.IO
import System.IO.Temp

makeCover :: String -> String -> String -> Int64 -> IO BS.ByteString
makeCover infoUnique infoTitle infoAuthor textSize = do

  let
    -- The color of the background is based on how long the story is.
    color = log (fromIntegral textSize) / log (10^9)

    -- This is the size of an A5 sheet in mm.
    width  = 148
    height = 210

    -- Density of image.
    pixelPerMM = 5

    widthPoints  = fromIntegral width  * 2.8346457
    heightPoints = fromIntegral height * 2.8346457

    -- Remaining units are in points.
    margin = 18

    smallFontSize = 48
    largeFontSize = 60

  authorFont <- fontDescriptionNew
  fontDescriptionSetFamily authorFont "Serif"
  fontDescriptionSetSize authorFont smallFontSize
  fontDescriptionSetWeight authorFont WeightNormal

  titleFont <- fontDescriptionNew
  fontDescriptionSetFamily titleFont "Serif"
  fontDescriptionSetSize titleFont largeFontSize
  fontDescriptionSetWeight titleFont WeightBold

  uniqueFont <- fontDescriptionNew
  fontDescriptionSetFamily uniqueFont "Serif"
  fontDescriptionSetSize uniqueFont smallFontSize
  fontDescriptionSetWeight uniqueFont WeightNormal

  withSystemTempFile "cover.png" $ \tempPath tempHandle -> do

    hClose tempHandle

    withImageSurface FormatRGB24 (width * pixelPerMM) (height * pixelPerMM) $ \surface -> do

      renderWith surface $ do

        -- Scale to points.
        scale (fromIntegral pixelPerMM * 0.35277778) (fromIntegral pixelPerMM * 0.35277778)

        pango <- liftIO $ cairoCreateContext Nothing
        liftIO $ cairoContextSetResolution pango 72 -- A point being, by definition, 1/72 inch.

        setSourceRGB 0 color color
        paint

        setSourceRGB 0 0 0

        -- The title.

        moveTo margin margin

        title <- liftIO $ layoutText pango infoTitle
        liftIO $ layoutSetAlignment title AlignCenter
        liftIO $ layoutSetFontDescription title $ Just titleFont
        liftIO $ layoutSetJustify title True
        liftIO $ layoutSetWidth title $ Just $ widthPoints - margin * 2
        liftIO $ layoutSetWrap title WrapPartialWords

        (_, PangoRectangle titleOffX titleOffY titleWidth titleHeight) <- liftIO $ layoutGetExtents title

        showLayout title

        -- The unique identifier.

        moveTo margin (margin + titleHeight + 2 * margin)

        unique <- liftIO $ layoutText pango infoUnique
        liftIO $ layoutSetAlignment unique AlignCenter
        liftIO $ layoutSetFontDescription unique $ Just uniqueFont
        liftIO $ layoutSetJustify unique True
        liftIO $ layoutSetWidth unique $ Just $ widthPoints - margin * 2
        liftIO $ layoutSetWrap unique WrapPartialWords

        (_, PangoRectangle uniqueOffX uniqueOffY uniqueWidth uniqueHeight) <- liftIO $ layoutGetExtents unique

        showLayout unique

        -- The author.

        moveTo margin (margin + titleHeight + 2 * margin + uniqueHeight + 2 * margin)

        author <- liftIO $ layoutText pango infoAuthor
        liftIO $ layoutSetAlignment author AlignCenter
        liftIO $ layoutSetFontDescription author $ Just authorFont
        liftIO $ layoutSetJustify author True
        liftIO $ layoutSetWidth author $ Just $ widthPoints - margin * 2
        liftIO $ layoutSetWrap author WrapPartialWords

        (_, PangoRectangle authorOffX authorOffY authorWidth authorHeight) <- liftIO $ layoutGetExtents author

        showLayout author

      surfaceWriteToPNG surface tempPath

    BS.readFile tempPath
