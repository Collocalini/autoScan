

module PatternRecognition (
checker_pattern_areas,
check_pattern,
detect_bookmarks,
detect_bookmarks_maybe,
detect_bookmark_maybe,
detect_bookmarks_maybe_io,
detect_bookmark_maybe_io,
detect_white_page,
detect_white_page_maybe_io

) where

import qualified Codec.Picture as CPic
import qualified Data.List as DList
import qualified Data.Vector.Storable as DVSec
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
   --cellw :: Rational
   cellw = (fromIntegral w)/ (fromIntegral sh)
   --cellh :: Rational
   cellh = (fromIntegral h)/ (fromIntegral sv)
   width = fromIntegral w
   height = fromIntegral h
   --getX1s :: [Rational]

   get_rects1 = [(x1, y1) | x1 <- [0,cellw .. width-cellw], y1 <- [0, cellh .. height-cellh]]

   get_rects2 = [(x2, y2) | x2 <- [cellw, 2*cellw .. width], y2 <- [cellh, 2*cellh .. height]]
----------------------------------------------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
average_areas :: CPic.Image CPic.Pixel8 -> [(Int, Int, Int, Int)] -> [Float]
average_areas img ars = --DArray.array ((1,1),(5,5)) [((x,y), 1) | x <- [0 .. w], y <- [0 .. h]]
                        map (average_area img) ars
  --where
  --w = 5
 -- h = 5

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




{-- ================================================================================================
================================================================================================ --}
check_pattern :: [Float] -> Bool
check_pattern [] = False
check_pattern [a,b] = a/=b
check_pattern (a:b:rest)
  |a<b = DList.and $ map (\(p,f) -> step1 f p) $ zip pattern1 $ step2 $ b:rest
  |a>b = DList.and $ map (\(p,f) -> step1 f p) $ zip pattern2 $ step2 $ b:rest
  |otherwise = False
  where
  step1 :: (Float, Float) -> Bool -> Bool
  step1 (l,r) False = l<r
  step1 (l,r) True = l>r

  step2 :: [Float] -> [(Float, Float)]
  step2 [] = []
  step2 [a] = []
  step2 (a:b:rest) = (a,b):(step2 $ b:rest)

  pattern1 = DList.cycle [True, False]
  pattern2 = DList.cycle [False, True]

----------------------------------------------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
detect_bookmark :: CPic.Image CPic.Pixel8 -> Bool
detect_bookmark image = (check_pattern.(\i -> average_areas i (cp i))) image
  where
  cp :: CPic.Image CPic.Pixel8 -> [(Int, Int, Int, Int)]
  cp img@(CPic.Image { CPic.imageWidth = w, CPic.imageHeight = h }) =
                           checker_pattern_areas (fromIntegral w-1) (fromIntegral h-1) 5 5
                                       --  checker_pattern_areas 2482 3507 5 5

detect_bookmark_maybe :: Maybe (CPic.Image CPic.Pixel8) -> Maybe Bool
detect_bookmark_maybe Nothing = Nothing
detect_bookmark_maybe (Just image) = Just $ detect_bookmark image

detect_bookmark_maybe_io :: IO (Maybe (CPic.Image CPic.Pixel8)) -> IO (Maybe Bool)
detect_bookmark_maybe_io image = do
                                      i <- image
                                      return $ detect_bookmark_maybe i
----------------------------------------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
detect_bookmarks :: [CPic.Image CPic.Pixel8] -> [Bool]
detect_bookmarks [] = []
detect_bookmarks images = map detect_bookmark images

detect_bookmarks_maybe :: [Maybe (CPic.Image CPic.Pixel8)] -> [Maybe Bool]
detect_bookmarks_maybe [] = []
detect_bookmarks_maybe images = map detect_bookmark_maybe images

detect_bookmarks_maybe_io :: IO [(Maybe (CPic.Image CPic.Pixel8))] -> IO [Maybe Bool]
--detect_bookmarks_maybe_io [] = return []
detect_bookmarks_maybe_io images = do
                                      i <- images
                                      return $ detect_bookmarks_maybe i
----------------------------------------------------------------------------------------------------






{-- ================================================================================================
================================================================================================ --}
detect_white_page :: CPic.Image CPic.Pixel8 -> Int -> Bool
detect_white_page (CPic.Image {CPic.imageWidth = _,
                               CPic.imageHeight = _,
                               CPic.imageData = image}) l = (abs cross - median) <= limit
   where
   ar = DVSec.toList image
   ars = DList.sort ar
   limit = fromIntegral l
   min = head ars
   max = last ars
   cross = div (max - min) 2
   median = DList.head $ DList.drop (div (DList.length ar) 2) ars


detect_white_page_maybe :: Maybe (CPic.Image CPic.Pixel8) -> Maybe Int -> Maybe Bool
detect_white_page_maybe Nothing Nothing = Nothing
detect_white_page_maybe _ Nothing = Nothing
detect_white_page_maybe Nothing _ = Nothing
detect_white_page_maybe (Just image) (Just limit)= Just $ detect_white_page image limit


detect_white_page_maybe_io :: IO (Maybe (CPic.Image CPic.Pixel8)) -> Maybe Int -> IO (Maybe Bool)
detect_white_page_maybe_io image limit = do
                                      i <- image

                                      return $ detect_white_page_maybe i limit
----------------------------------------------------------------------------------------------------







