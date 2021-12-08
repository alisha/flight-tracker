{-. LANGUAGE QuasiQuotes .-}
module USA (
  convertPixelToAsciiCoordinates,
  mercatorMap,
  mercatorMapPixelTotalWidth,
  mercatorMapPixelTotalHeight,
  mercatorMapPixelOriginWidth,
  mercatorMapPixelOriginHeight,
  mapCharWidth,
  mapCharHeight
)
where


-- The information in this file was created with the help of:
-- https://cloudapps.herokuapp.com/imagetoascii/
-- https://stackoverflow.com/questions/14329691/convert-latitude-longitude-point-to-a-pixels-x-y-on-mercator-projection
-- https://mathworld.wolfram.com/MercatorProjection.html
-- https://github.com/mfeldheim/hermap/blob/master/examples/resources/Normal_Mercator_map_85deg.jpg

{-
A short list of steps which was followed in order to generate this data

1. download the image from the github repository
2. put into GIMP and crop onto the USA. Mark the crop size as well as at least
the top-left or bottom-right coordinate location.
3. convert the image to grayscale in GIMP after cropping and upload onto the
imagetoascii web application
4. update the charset to custom and enter "@/."
5. finally, copy the ASCII generated to this file, and then enter all of the
the values for each of the defined variables.
-}


mercatorMapPixelTotalWidth :: Float
mercatorMapPixelTotalWidth = 1200
mercatorMapPixelTotalHeight :: Float
mercatorMapPixelTotalHeight = 1200
mercatorMapPixelOriginWidth :: Float
mercatorMapPixelOriginWidth = 166
mercatorMapPixelOriginHeight :: Float
mercatorMapPixelOriginHeight = 406

mapCharWidth :: Int
mapCharWidth = 120
mapCharHeight :: Int
mapCharHeight = 23

mapPixelWidth :: Int
mapPixelWidth = 234
mapPixelHeight :: Int
mapPixelHeight = 126

convertPixelToAsciiCoordinates :: (Float, Float) -> (Int, Int)
convertPixelToAsciiCoordinates (x, y) = (fromIntegral xAscii :: Int, fromIntegral yAscii :: Int)
  where
    xAscii = floor ((x * fromIntegral mapCharWidth) / fromIntegral mapPixelWidth)
    yAscii = floor ((y * fromIntegral mapCharHeight) / fromIntegral mapPixelHeight)


-- cropped region size
-- 234x126
-- bottom right pixel coordinate
-- 400x532
-- original map pixel size
-- 1200x1200
-- (derived) top-left pixel coordinate from original image:
-- x: (400-234) = 166
-- y = (532-126) = 406
mercatorMap :: String
mercatorMap = "\
\@@@@@@...........................................................................................................@..@.@.\n\
\@@@@@@@@@........................................................................................................@@@@@@@\n\
\@@@@@@@@@...@.................................................................................................@@.@@@.@@@\n\
\@@@@@@@@@@......................................................................................................@@.@....\n\
\@@@@@@@@@........................................................................................@.......@.@.@....@@@@@@\n\
\@@@@@@@@@...............................................................................................@@@@@.@@@@@@@.@@\n\
\@@@@@@@@@............................................................................................@..@@@@@@@@@@@@@@@@\n\
\@@@@@@@@@@........................................................................................@@@@..@..@@@@@@@@@@@@@\n\
\@@@@@@@@@@..........................................................................................@@..@@@@@@@@@@@@@@@@\n\
\@@@@@@@@@@@..@.............................................................................@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\
\@@@@@@@@@@@@@.............................................................................@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\
\@@@@@@@@@@@@@@............................................................................@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\
\@@@@@@@@@@@@@@@@.................................................@@@...................@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\
\@@@@@@@@@@@@@@@@@.@@............................................@@@@...@...........@@..@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\
\@@@@@@@@@@@@@@@@@@@@@@@..@......................................@@@....@@.......@.@@@..@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\
\@@@@@@@@@@@@@@@@@@@@@@@@..@@@...............................@@@@@@.@..@@@@@@.@@@@@@@@..@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\
\@@@@@@@@@@@@@@@@@@@@@@@@@..................................@.......@....@@@............@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\
\@@@@@@@@@@@@@@@@@@@@@@@@@@....@@@......................@@@@@@@@@@@@@.@@@@@@@@@...@@@@..@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\
\@@@@@@@@@@@@@@@@@@@@@@@@@@@@@..@@@.@..................@@@@@@@@@@@@@@@@@@@@@@@@@...@@@..@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\
\@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@..@@@@@.................@@@@@@@@@@@@@@@@@@@@@@@@@...@@.@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\
\@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@.@@@@@...............@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@..@.@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\
\@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@............@@@@@@@@@@@@@@......@@@@@@@@@@@..@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\
\@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@..............@@@@@@@@@.......@@@@@@@@@@@@@@@@@...@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n\
\"
