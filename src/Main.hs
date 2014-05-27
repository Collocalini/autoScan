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
import Control.DeepSeq

---from this project
import Global
import PatternRecognition
---end of imports from this project



generate_concat_prescription :: IO ()
generate_concat_prescription = do
   files <- ls "./ls_script" ""
   --let images =
   --let detect f = (detect_bookmarks_maybe_io $ mapM (rgb2grayscale_io_maybe.loadImage) f)


   --(detect $ take 3 files) >>=
   --  (\d -> putStr $ unlines $ step2 (zip  d $ take 3 files) 0)
   step1 files 0
   --return ()
   where

   step1 :: [FilePath] -> Int -> IO ()
   step1 [] _ = return ()
   step1 (f:rest) i  = do
     dl <- detect f
     case dl of
      (Just False) -> do
                       putStrLn $ (take ((length f)-3) f ++ "pdf   auto" ++ show i)
                       step1 rest i
                  --   return ()
      (Nothing) ->    do
                       putStrLn $ (take ((length f)-3) f ++ "pdf   auto" ++ show i)
                       step1 rest i
                  --   return ()
      (Just True) ->  i `deepseq` (step1 rest $ i+1)

      where
      detect f = (detect_bookmark_maybe_io $ (rgb2grayscale_io_maybe.loadImage) f)

      --to_take = take 1 f

   {--
   step2 :: [(Maybe Bool,FileName)] -> Int -> [String]
   step2 [] _ = []
   step2 ((Just False, f):rest) i = (take ((length f)-3) f ++ "pdf   auto" ++ show i):(step2 rest i)
   step2 ((Nothing   , f):rest) i = (take ((length f)-3) f ++ "pdf   auto" ++ show i):(step2 rest i)
   step2 ((Just True , f):rest) i = i `deepseq` (step2 rest $ i+1)
--}

{-- ================================================================================================
================================================================================================ --}
routine:: [String] -> IO ()
routine args
  |otherwise = --return ()
               generate_concat_prescription
  where

    tag_DMap' = tag_DMap args

----------------------------------------------------------------------------------------------------


main = do

    getArgs >>= routine
    --putStr ""
    --test1


-----   -static -optl-static -optl-pthread
