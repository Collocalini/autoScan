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
import Control.Monad.Writer
import Control.Monad.Reader
import Control.DeepSeq
import qualified Codec.Picture as CPic

---from this project
import Global
import PatternRecognition
---end of imports from this project


data PageMark = Page|WhitePage|FaultyPage|Separator



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




analyse ::  ReaderT InputArgs IO [(FilePath, Int, PageMark)]
analyse = do

   (InputArguments {scripts_folder = scripts_folder'}) <- ask
   files <- lift $ ls (scripts_folder' ++ "/ls_script") ""
   lift $ step1 files 0
  --step1 [] 0
   where

   step1 :: [FilePath] -> Int -> IO [(FilePath, Int, PageMark)]
   step1 [] _ = return []
   step1 (f:rest) i  = do
     dl <- detect f
     case dl of
      (Just False) -> do
                       putStrLn $ (take ((length f)-3) f ++ "pdf   auto" ++ show i)
                       return $ (f,i,Page)
                       step1 rest i

      (Nothing) ->    do
                       putStrLn $ (take ((length f)-3) f ++ "pdf   fault" ++ show i)
                       return $ (f,i,FaultyPage)
                       step1 rest i

      (Just True) ->  do
                       putStrLn $ (take ((length f)-3) f ++ "pdf   separator" ++ show i)
                       return $ (f,i,Separator)
                       i `deepseq` (step1 rest $ i+1)

      where
      detect f = (detect_bookmark_maybe_io $ (to_grayscale_io_maybe.loadImage) f)




groupByFile :: [(FilePath, Int, PageMark)] -> [[(FilePath, Int, PageMark)]]
groupByFile f = groupBy mark f
  where
  mark :: (FilePath, Int, PageMark) -> (FilePath, Int, PageMark) -> Bool
  mark (_, i, _) (_, i1, _) = i == i1



weedWhitePages :: [(FilePath, Int, PageMark)] ->
                 ([(FilePath, Int, PageMark)],[(FilePath, Int, PageMark)])
weedWhitePages f = partition iswhite f
  where
  iswhite (_, _, WhitePage) = True
  iswhite (_, _, _)         = False



{-- ================================================================================================
================================================================================================ --}
routine:: [String] -> IO ()
routine args
  |otherwise = --return ()
               generate_concat_prescription "."
  where


    tag_DMap' = tag_DMap args

----------------------------------------------------------------------------------------------------


main = do

    getArgs >>= routine
    --putStr ""
    --test1


-----   -static -optl-static -optl-pthread
