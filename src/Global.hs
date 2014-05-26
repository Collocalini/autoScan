
module Global (



flags ,

options,

tag_DMap,

list_arguments,

eol_char,

read_file_if_exists,

FileName,

strToFilename,

get_comma_separated,


) where

import qualified Data.Map as DMap
import qualified Codec.Picture as CPic
--import Data.List
--import System.Process
import System.IO
--import System.Directory
--import Data.Time
--import System.Locale



flags = [

        ]

options =  [

           ]

{-- ================================================================================================
================================================================================================ --}
tag_DMap:: [String] -> DMap.Map String String
tag_DMap [] = DMap.fromList [
        --("",""),


   ]----]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]

tag_DMap lst = DMap.union (DMap.fromList $ map (\(Just x) -> x) $ list_arguments lst) $
                                                                                       tag_DMap []
----------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
list_arguments :: [String] -> [Maybe (String, String)]
list_arguments [] = []
list_arguments (tag:rest)
  | take 2 tag == "--" && elem tag' flags =
                       (Just (tag', "true")) : list_arguments rest
  | take 2 tag == "--" && elem tag' options =
                       (Just (tag', after_tag)) : list_arguments rest'

  |otherwise = list_arguments rest

  where
     after_tag = head rest
     tag' = (drop 2 tag)

     rest'
        |rest /= [] = tail rest
        |otherwise = []
     rest''
        |rest' /= [] = tail rest'
        |otherwise = []
----------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
get_comma_separated :: String -> [String]
get_comma_separated arg = words $ map commas2spaces arg
   where
    commas2spaces :: Char -> Char
    commas2spaces c
       |c == ',' = ' '
       |otherwise = c
--------------------------------------------------------------------------------------------------




eol_char = "\n"

type FileName = String

strToFilename :: String -> FileName
strToFilename s = s









{-- ================================================================================================
================================================================================================ --}
read_file_if_exists :: FilePath -> IO String
read_file_if_exists [] = do return ""
read_file_if_exists name  = do
       handle <- openFile name ReadMode
       c <- hGetContents handle
       return c
-------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
loadImage :: FilePath -> IO (Maybe (CPic.Image CPic.PixelRGB8))
loadImage name = do image <- CPic.readImage name
                    case image of
                      (Left s) -> do
                                    print s
                                    return Nothing
                                     --exitWith (ExitFailure 1)
                      (Right d) ->
                                 do
                                    return  $ Just $ (\(CPic.ImageRGB8 i) -> i) d
                                    --return  $ Just $ CPic.pixelAt ((\(CPic.ImageRGB8 i) -> i) d) 0 0
                                    --return $ Just d

       --(Maybe CPic.PixelRGB8) --(Maybe CPic.DynamicImage)
----------------------------------------------------------------------------------------------------




rgb2grayscale :: CPic.Image CPic.PixelRGB8 -> CPic.Image CPic.Pixel8
rgb2grayscale img = CPic.pixelMap step1 img
  where
  step1 :: CPic.PixelRGB8 -> CPic.Pixel8
  step1 (CPic.PixelRGB8 r g b) = div (r+g+b) 3



