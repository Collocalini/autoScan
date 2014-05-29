-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  PublicDomain
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (

main

) where

--import System.IO
import System.Environment
--import System.Directory
import Data.List
--import qualified Data.Set as DSet
--import qualified Data.Map as DMap
--import Control.Monad.Writer
import Control.Monad.Reader
import Control.DeepSeq
import qualified Codec.Picture as CPic

---from this project
import Global
import PatternRecognition
---end of imports from this project





{-- ================================================================================================
================================================================================================ --}
generate_concat_prescription :: String -> IO ()
generate_concat_prescription scripts_folder = do
   files <- ls (scripts_folder ++ "/ls_script") ""
   step1 files 0

   where

   step1 :: [FilePath] -> Int -> IO ()
   step1 [] _ = return ()
   step1 (f:rest) i  = do
     dl <- detect f
     case dl of
      (Just False) -> do
                       putStrLn $ (take ((length f)-3) f ++ "pdf   auto" ++ show i)
                       step1 rest i

      (Nothing) ->    do
                       putStrLn $ (take ((length f)-3) f ++ "pdf        auto" ++ show i)
                       step1 rest i

      (Just True) ->  i `deepseq` (step1 rest $ i+1)

      where
      detect f = (detect_bookmark_maybe_io $ (to_grayscale_io_maybe.loadImage) f)
----------------------------------------------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
analyse ::  ReaderT InputArguments IO [(FilePath, Int, PageMark)]
analyse = do

   (InputArguments {scripts_folder = scripts_folder',
                    mark_with = mark_with'
                   }) <- ask
   files <- lift $ ls (scripts_folder' ++ "/ls_script") ""
   lift $ step1 files 0 mark_with'
  --step1 [] 0
   where

   step1 :: [FilePath] -> Int -> String -> IO [(FilePath, Int, PageMark)]
   step1 [] _ _ = return []
   step1 (f:rest) i mark = do
     dl <- detect f
     case dl of
      (Just False) -> do
                       putStrLn $ f ++ "   " ++ mark ++ show i
                       --return (f,i,Page)
                       s <- (step1 rest i mark)
                       return $ (f,i,Page):s

      (Nothing) ->    do
                       putStrLn $ f ++ "   fault-" ++ mark ++ show i
                       --return (f,i,FaultyPage)
                       --step1 rest i mark
                       s <- (step1 rest i mark)
                       return $ (f,i,FaultyPage):s
      (Just True) ->  do
                       putStrLn $ f ++ "   separator-" ++ mark ++ show i
                       --return (f,i,Separator)
                       --i `deepseq` (step1 rest (i+1) mark)
                       s <- i `deepseq` (step1 rest (i+1) mark)
                       return $ (f,i,Separator):s
      --(_) ->  putStr ""

      where
      detect f = (detect_bookmark_maybe_io $ (to_grayscale_io_maybe.loadImage) f)
----------------------------------------------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
groupByFile :: [(FilePath, Int, PageMark)] -> [[(FilePath, Int, PageMark)]]
groupByFile f = groupBy mark f
  where
  mark :: (FilePath, Int, PageMark) -> (FilePath, Int, PageMark) -> Bool
  mark (_, i, _) (_, i1, _) = i == i1
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
weedWhitePages :: [(FilePath, Int, PageMark)] ->
                 ([(FilePath, Int, PageMark)],[(FilePath, Int, PageMark)])
weedWhitePages f = partition iswhite f
  where
  iswhite (_, _, WhitePage) = True
  iswhite (_, _, _)         = False
----------------------------------------------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
save_analysis_stage ::  [(FilePath, Int, PageMark)] -> ReaderT InputArguments IO ()
save_analysis_stage a = do
  (InputArguments {analysis_results_file = analysis_results_file',
                   analysis_detected_pages_to = analysis_detected_pages_to',
                   analysis_white_pages_to = analysis_white_pages_to'
                   --mark_with = mark_with'
                  }) <- ask

  lift $ putStrLn $ "save_analysis_stage" ++ show analysis_results_file'
  lift $ save_analysis_results_file analysis_results_file' a -- $ groupByFile a
  lift $ save_detected_and_or_white_pages analysis_detected_pages_to' analysis_white_pages_to' a
  --return ()
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
save_analysis_results_file :: Maybe FilePath -> [(FilePath, Int, PageMark)] -> IO ()
save_analysis_results_file Nothing _ = return ()
save_analysis_results_file (Just f) a = do
   putStrLn $ "save_analysis_results_file" ++ (unlines $ map step1 a)

   writeFile f $ unlines $ map step1 a
   where
   step1 :: (FilePath, Int, PageMark) -> String
   step1 (fp, i, pm) = fp ++ "   " ++ show i ++ "   " ++ show pm
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
save_detected_and_or_white_pages :: Maybe FilePath -> Maybe FilePath ->
                                    [(FilePath, Int, PageMark)] -> IO ()
save_detected_and_or_white_pages fd fw a = do
   case (fw,fd) of
     (Just fw,Nothing)  -> writeFile fw $ unlines $ (\(w, _) -> map step1 w) $ weedWhitePages a
     (Nothing, Just fd) -> writeFile fd $ unlines $ (\(_, d) -> map step1 d) $ weedWhitePages a
     (Just fw, Just fd) -> do
                             let (w,d) = weedWhitePages a
                             writeFile fw $ unlines $ map step1 w
                             writeFile fd $ unlines $ map step1 d
     (_) -> return ()
   where
   step1 (fp, i, pm) = fp ++ "   " ++ show i ++ "   " ++ show pm

----------------------------------------------------------------------------------------------------






{-- ================================================================================================
================================================================================================ --}
use_analysis_stage ::  [(FilePath, Int, PageMark)] -> ReaderT InputArguments IO ()
use_analysis_stage a = do
  (InputArguments {white_pages_to_pdf = white_pages_to_pdf'
                  ,detected_pages_to_pdf = detected_pages_to_pdf'
                  ,scans_to_pdfs = scans_to_pdfs'
                   --mark_with = mark_with'
                  }) <- ask

  case (white_pages_to_pdf',detected_pages_to_pdf',scans_to_pdfs') of
     (Nothing,Nothing, Just scans_to_pdfs')  -> do
        lift $ writeFile "./scans_to_pdfs" $ unlines $ step1 scans_to_pdfs' $
                                                       (\(_, d) -> groupByFile d) $ weedWhitePages a
        lift $ run_script "./scans_to_pdfs"
     (_) -> return ()

  return ()
  where
  step1 :: FilePath -> [[(FilePath, Int, PageMark)]] -> [String]
  step1 _ [] = []
  step1 f ([]:rest) = step1 f rest
  step1 f (a@((_, i, _):_):rest) = ((gs f i) ++ (concat $ step2 a)):(step1 f rest)
    where
      step2 [] = []
      step2 ((fp, _, Page):rest) = ("'" ++ take ((length fp)-3) fp ++ "pdf'  ") : (step2 rest)
      step2 ((fp, _, FaultyPage):rest) = ("'" ++ take ((length fp)-3) fp ++ "pdf'  ") : (step2 rest)
      step2 ((fp, _, _):rest) = step2 rest
      gs f i = "gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=" ++ f ++ show i ++ ".pdf "
----------------------------------------------------------------------------------------------------





{-- ================================================================================================
================================================================================================ --}
read_analysis_file_Ia :: ReaderT InputArguments IO [(FilePath, Int, PageMark)]
read_analysis_file_Ia =  do
   (InputArguments {analysis_results_file = analysis_results_file'}) <- ask
   f <- lift $ read_file_if_exists $ step1 analysis_results_file'
   lift $ (return . read_analysis_file . (map words) . lines) f
   where
   step1 :: Maybe FilePath -> String
   step1 (Just f) = f
   step1 Nothing = []
----------------------------------------------------------------------------------------------------






{-- ================================================================================================
================================================================================================ --}
we_need_to_go_DEEPER :: ReaderT InputArguments IO ()
we_need_to_go_DEEPER = do
  (InputArguments {perform = perform'}) <- ask
 -- lift $ putStrLn $ "we_need_to_go_DEEPER" ++ show perform'
  step1 $ perform'
 -- lift $ putStr ""
  where

  step1 :: Maybe [Perform] -> ReaderT InputArguments IO ()
  step1 Nothing = step2a
  step1 (Just []) = step2a
  step1 (Just p) = step2 $ prepareActions p


-----------------------


  step2 :: [Perform] -> ReaderT InputArguments IO ()
  step2 [Analyse] = step2a
  step2 [Analyse,SaveAnalysisResults] = step2b
  step2 [Analyse,UseAnalysisResults] = step2bb
  step2 [Analyse,SaveAnalysisResults,UseAnalysisResults] = step2c


  step2 [ReadAnalysis] = return ()
  step2 [ReadAnalysis,SaveAnalysisResults] = step2b1
  step2 [ReadAnalysis,UseAnalysisResults] = step2bb1
  step2 [ReadAnalysis,SaveAnalysisResults,UseAnalysisResults] = step2c1

  step2a = do analyse
              return ()
  step2b = save_analysis_stage =<<  analyse

  step2bb = use_analysis_stage =<<  analyse

  step2c = do a <- analyse
              save_analysis_stage a
              use_analysis_stage a

  step2b1 = save_analysis_stage =<< read_analysis_file_Ia

  step2bb1 = use_analysis_stage =<<  read_analysis_file_Ia

  step2c1 = do a <- read_analysis_file_Ia
               save_analysis_stage a
               use_analysis_stage a
--------------------------------------


  prepareActions :: [Perform] -> [Perform]
  --prepareActions [] = []
  prepareActions p
    |any haveAnalyse p && any haveReadAnalysis p = []
    |any haveAnalyse p && (not $ any haveReadAnalysis p) = (Analyse):(prepareActions_a p)
    |(not $ any haveAnalyse p) && (any haveReadAnalysis p) = (ReadAnalysis):(prepareActions_a p)
    |(not $ any haveAnalyse p) && (not $ any haveReadAnalysis p) = []
    |otherwise = []
    where
    prepareActions_a :: [Perform] -> [Perform]
    --prepareActions_a [] = []
    prepareActions_a p
     |(any haveSaveAnalysisResults p) =
       (\(_,r) -> (SaveAnalysisResults):prepareActions_a r) $ partition (haveSaveAnalysisResults) p
     |(any haveUseAnalysisResults p) =
       (\(_,r) -> (UseAnalysisResults):prepareActions_a r) $ partition (haveUseAnalysisResults) p
     |otherwise = []

  haveAnalyse :: Perform -> Bool
  haveAnalyse Analyse = True
  haveAnalyse _ = False

  haveReadAnalysis :: Perform -> Bool
  haveReadAnalysis ReadAnalysis = True
  haveReadAnalysis _ = False

  haveSaveAnalysisResults :: Perform -> Bool
  haveSaveAnalysisResults SaveAnalysisResults = True
  haveSaveAnalysisResults _ = False

  haveUseAnalysisResults :: Perform -> Bool
  haveUseAnalysisResults UseAnalysisResults = True
  haveUseAnalysisResults _ = False

----------------------------------------------------------------------------------------------------

--}




{-- ================================================================================================
================================================================================================ --}
routine:: [String] -> IO ()
routine args
  |args /= [] = do runReaderT we_need_to_go_DEEPER $ inputArgs $ tag_DMap args
                 -- return ()
  |otherwise = return ()
          --     generate_concat_prescription "."
  where
 --   step1 :: ()
 --   step1 =

    --tag_DMap' = tag_DMap args

----------------------------------------------------------------------------------------------------


main = do

    getArgs >>= routine
    --putStr ""
    --test1


-----   -static -optl-static -optl-pthread
