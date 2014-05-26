

module PatternRecognition (

) where

import qualified Codec.Picture as CPic
--import qualified Codec.Picture.Types as CPicT

-- array                   :: (Ix a) => (a,a) -> [(a,b)] -> Array a b
-- squares                 =  array (1,100) [(i, i*i) | i <- [1..100]]





{-- ================================================================================================
================================================================================================ --}
checker_pattern_areas :: Int -> Int -> Int -> Int -> [(Int, Int, Int, Int)]
checker_pattern_areas w h sh sv =
   map (\((x1, y1), (x2, y2)) -> (round x1, round y1, round x2, round y2) ) $
                                                                           zip get_rects1 get_rects2
   where
   cellw :: Rational
   cellw = (fromIntegral w)/ (fromIntegral sh)
   cellh :: Rational
   cellh = (fromIntegral h)/ (fromIntegral sv)
   width = fromIntegral w
   height = fromIntegral h
   --getX1s :: [Rational]

   get_rects1 = [(x1, y1) | x1 <- [0,cellw .. width-cellw], y1 <- [0, cellh .. height-cellh]]

   get_rects2 = [(x2, y2) | x2 <- [cellw, 2*cellw .. width], y2 <- [cellh, 2*cellh .. height]]
----------------------------------------------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
----------------------------------------------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
average_area :: CPic.Image CPic.Pixel8 -> (Int, Int, Int, Int) -> Float
average_area img area@(x1, y1, x2, y2) =  (fromIntegral $ step1 $ allCoords area) / area_size
   where
   area_size :: Float
   area_size = fromIntegral ((abs $ x2 - x1) * (abs $ y2 - y1))

   step1 :: [(Int, Int)] -> Int
   step1 [] = 0
   step1 ((x,y):r) = (fromIntegral $ CPic.pixelAt img x y) + (step1 r)
----------------------------------------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
allCoords :: (Int, Int, Int, Int) -> [(Int, Int)]
allCoords (x1, y1, x2, y2) = [(x,y) | x <- [x1..x2], y <- [y1..y2]]
----------------------------------------------------------------------------------------------------



