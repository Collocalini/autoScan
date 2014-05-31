
module Global (

PageMark(..),
Perform(..),
InputArguments(..),
inputArgs,

--flags ,

--options,

tag_DMap,

list_arguments,

eol_char,

read_file_if_exists,

FileName,

strToFilename,

get_comma_separated,

loadImage,

rgb2grayscale,
rgb2grayscale_io_maybe,
to_grayscale_io_maybe,


run_script,
ls,

read_analysis_file,

argument_perform


) where

import qualified Data.Map as DMap
import qualified Codec.Picture as CPic
--import Control.Monad.State
import Data.List
import System.Process
import System.IO
--import System.Directory
--import Data.Time
--import System.Locale

data PageMark = Page|WhitePage|FaultyPage|Separator deriving (Show, Read)
data Perform = Analyse|ReadAnalysis|SaveAnalysisResults|UseAnalysisResults deriving Show
data InputArguments = InputArguments {
                                      analysis_results_file :: Maybe FilePath
                                     ,analysis_detected_pages_to :: Maybe FilePath
                                     ,analysis_white_pages_to :: Maybe FilePath
                                     ,perform :: Maybe [Perform]
                                     ,mark_with :: String
                                     ,white_pages_to_pdf  :: Maybe FilePath
                                     ,detected_pages_to_pdf :: Maybe FilePath
                                     ,scans_to_pdfs :: Maybe FilePath
                                     ,scripts_folder :: FilePath
                                     ,white_page_tolerance :: Maybe Int
                                     }


{-- ================================================================================================
================================================================================================ --}
inputArgs :: DMap.Map String String -> InputArguments
inputArgs tm = InputArguments {

   analysis_results_file = analysis_results_file'

  ,analysis_detected_pages_to = analysis_detected_pages_to'

  ,analysis_white_pages_to = analysis_white_pages_to'

  ,perform = perform'

  ,mark_with = mark_with'

  ,white_pages_to_pdf = white_pages_to_pdf'

  ,detected_pages_to_pdf = detected_pages_to_pdf'

  ,scans_to_pdfs = scans_to_pdfs'

  ,scripts_folder = scripts_folder'

  ,white_page_tolerance = white_page_tolerance'
  }
  where
  analysis_results_file'
     |s/= default_analysis_results_file = Just s
     |otherwise = Nothing
     where
     s = (DMap.findWithDefault default_analysis_results_file argument_analysis_results_file tm)


  analysis_detected_pages_to'
     |s/= default_analysis_detected_pages_to = Just s
     |otherwise = Nothing
     where
     s = (DMap.findWithDefault default_analysis_detected_pages_to
                                                             argument_analysis_detected_pages_to tm)

  analysis_white_pages_to'
     |s/= default_analysis_white_pages_to = Just s
     |otherwise = Nothing
     where
     s = (DMap.findWithDefault default_analysis_white_pages_to
                                                             argument_analysis_white_pages_to tm)

  perform'
     |s/= default_perform = Just $ strListToPerform $ get_comma_separated s
     |otherwise = Nothing
     where
     s = (DMap.findWithDefault default_perform argument_perform tm)

  mark_with' = (DMap.findWithDefault default_mark_with argument_mark_with tm)

  white_pages_to_pdf'
     |s/= default_white_pages_to_pdf = Just s
     |otherwise = Nothing
     where
     s = (DMap.findWithDefault default_white_pages_to_pdf argument_white_pages_to_pdf
                                                             tm)

  detected_pages_to_pdf'
     |s/= default_detected_pages_to_pdf = Just s
     |otherwise = Nothing
     where
     s = (DMap.findWithDefault default_detected_pages_to_pdf
                                                             argument_detected_pages_to_pdf tm)

  scans_to_pdfs'
     |s/= default_scans_to_pdfs = Just s
     |otherwise = Nothing
     where
     s = (DMap.findWithDefault default_scans_to_pdfs argument_scans_to_pdfs tm)

  scripts_folder' = (DMap.findWithDefault default_scripts_folder argument_scripts_folder tm)

  white_page_tolerance'
    |s /= "" = Just $ read s
    |s == "" = Nothing
    where
    s= (DMap.findWithDefault default_white_page_tolerance argument_white_page_tolerance tm)
----------------------------------------------------------------------------------------------------









argument_analysis_results_file = "analysis-results-file"
argument_analysis_detected_pages_to = "analysis-detected-pages-to"
argument_analysis_white_pages_to = "analysis-white-pages-to"
argument_perform = "perform"
argument_mark_with = "mark-with"
argument_white_pages_to_pdf = "white-pages-to-pdf"
argument_detected_pages_to_pdf = "detected-pages-to-pdf"
argument_scans_to_pdfs = "scans-to-pdfs"
argument_scripts_folder = "scripts-folder"
argument_white_page_tolerance = "white-page-tolerance"


default_analysis_results_file = ""
default_analysis_detected_pages_to = ""
default_analysis_white_pages_to = ""
default_perform = ""
default_mark_with = "auto"
default_white_pages_to_pdf = ""
default_detected_pages_to_pdf = ""
default_scripts_folder = "."
default_scans_to_pdfs = ""
default_white_page_tolerance = ""



perform_stage_analyse = "analyse"
perform_stage_read_analysis = "read-analysis"
perform_stage_save_analysis_results = "save-analysis-results"
perform_stage_use_analysis_results = "use-analysis-results"




flags = [

        ]

options =  [
             argument_analysis_results_file,
             argument_analysis_detected_pages_to,
             argument_analysis_white_pages_to,
             argument_perform,
             argument_mark_with,
             argument_white_pages_to_pdf,
             argument_detected_pages_to_pdf,
             argument_scans_to_pdfs,
             argument_scripts_folder,
             argument_white_page_tolerance
           ]

{-- ================================================================================================
================================================================================================ --}
tag_DMap:: [String] -> DMap.Map String String
tag_DMap [] = DMap.fromList [
        --("",""),
          (argument_analysis_results_file,       default_analysis_results_file),
          (argument_analysis_detected_pages_to,  default_analysis_detected_pages_to),
          (argument_analysis_white_pages_to,     default_analysis_white_pages_to),
          (argument_perform,                     default_perform),
          (argument_mark_with,                   default_mark_with),
          (argument_white_pages_to_pdf,          default_white_pages_to_pdf),
          (argument_detected_pages_to_pdf,       default_detected_pages_to_pdf),
          (argument_scans_to_pdfs,               default_scans_to_pdfs),
          (argument_scripts_folder,              default_scripts_folder),
          (argument_white_page_tolerance,        default_white_page_tolerance)

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



{-- ================================================================================================
================================================================================================ --}
strListToPerform :: [String] -> [Perform]
strListToPerform [] = []
strListToPerform (s:rest)
  |s== perform_stage_analyse = Analyse:strListToPerform rest
  |s== perform_stage_read_analysis = ReadAnalysis:strListToPerform rest
  |s== perform_stage_save_analysis_results = SaveAnalysisResults:strListToPerform rest
  |s== perform_stage_use_analysis_results = UseAnalysisResults:strListToPerform rest
  |otherwise = strListToPerform rest
----------------------------------------------------------------------------------------------------



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
loadImage :: FilePath -> IO (Maybe (CPic.DynamicImage))--(Maybe (CPic.Image CPic.PixelRGB8))
loadImage name = do image <- CPic.readImage name
                    case image of
                      (Left s) -> do
                                    print s
                                    return Nothing
                                     --exitWith (ExitFailure 1)
                      (Right d) ->
                                 do
                                    return $ Just d -- $ fmt d
                                 --return  $ Just $ CPic.pixelAt ((\(CPic.ImageRGB8 i) -> i) d) 0 0
                                 --return $ Just d
  where
  fmt :: CPic.DynamicImage -> Maybe (CPic.Image CPic.PixelRGB8)
 -- fmt i
 --   |(CPic.ImageRGB8 i) = Just i
 --   |otherwise = Nothing
  fmt (CPic.ImageRGB8 i) = Just i
  fmt (_) = Nothing

       --(Maybe CPic.PixelRGB8) --(Maybe CPic.DynamicImage)
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
rgb2grayscale :: CPic.DynamicImage -> CPic.Image CPic.Pixel8
rgb2grayscale (CPic.ImageRGB8 img) = CPic.pixelMap step1 img
  where
  step1 :: CPic.PixelRGB8 -> CPic.Pixel8
  step1 (CPic.PixelRGB8 r g b) = div (r+g+b) 3


rgb2grayscale_maybe :: Maybe (CPic.DynamicImage) ->
                          Maybe (CPic.Image CPic.Pixel8)
rgb2grayscale_maybe Nothing = Nothing
rgb2grayscale_maybe (Just img) = Just $ rgb2grayscale img

rgb2grayscale_io_maybe :: IO (Maybe (CPic.DynamicImage)) ->
                          IO (Maybe ( CPic.Image CPic.Pixel8))
rgb2grayscale_io_maybe img = do i <- img
                                return $ rgb2grayscale_maybe i
----------------------------------------------------------------------------------------------------






{-- ================================================================================================
================================================================================================ --}
to_grayscale :: CPic.DynamicImage -> CPic.Image CPic.Pixel8
to_grayscale (CPic.ImageRGB8 img) = CPic.pixelMap step1 img
  where
  step1 :: CPic.PixelRGB8 -> CPic.Pixel8
  step1 (CPic.PixelRGB8 r g b) = div (r+g+b) 3


to_grayscale (CPic.ImageRGB16 img) = CPic.pixelMap step1 img
  where
  step1 :: CPic.PixelRGB16 -> CPic.Pixel8
  step1 (CPic.PixelRGB16 r16 g16 b16) = round $ ((rf+gf+bf) / 3) * ((2^8)/(2^16))
     where
     rf= fromIntegral r16
     gf= fromIntegral g16
     bf= fromIntegral b16

to_grayscale (CPic.ImageY8 img) = img


to_grayscale_maybe :: Maybe (CPic.DynamicImage) ->
                          Maybe (CPic.Image CPic.Pixel8)
to_grayscale_maybe Nothing = Nothing
to_grayscale_maybe (Just img) = Just $ to_grayscale img

to_grayscale_io_maybe :: IO (Maybe (CPic.DynamicImage)) ->
                          IO (Maybe ( CPic.Image CPic.Pixel8))
to_grayscale_io_maybe img = do i <- img
                               return $ to_grayscale_maybe i
----------------------------------------------------------------------------------------------------






{-- ================================================================================================
================================================================================================ --}
run_script :: String -> IO ()
run_script script = do (runCommand $ script ) >>= waitForProcess
                       return ()
----------------------------------------------------------------------------------------------------







{-- ================================================================================================
================================================================================================ --}
ls :: String -> -- ls
      String -> -- properties
  IO [String]
ls file properties  =
    do s <- readProcess (file ++ properties) [] ""
       --putStrLn $ (file ++ properties)
       return $ lines s
----------------------------------------------------------------------------------------------------






{-- ================================================================================================
================================================================================================ --}
read_analysis_file :: [[String]] -> [(FilePath, Int, PageMark)]
read_analysis_file f = step2 $ map step1 f
   where
   step1 :: [String] -> Maybe (FilePath, Int, PageMark)
   step1 [] = Nothing
   step1 [_,_] = Nothing
   step1 [a,b,c] = Just (a, read b, read c)
   step1 (a:b:c:rest) = (a ++ b) `seq` step1 $ (a ++ b):c:rest

   step2 [] = []
   step2 ((Just x):rest) = x:step2 rest
   step2 ((Nothing):rest) = step2 rest
----------------------------------------------------------------------------------------------------



