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

---from this project
import Global
import PatternRecognition
---end of imports from this project








{-- ================================================================================================
================================================================================================ --}
routine:: [String] -> IO ()
routine args
  |otherwise = --return ()
               do
                files <- ls "./ls_script" ""

                images <- mapM (rgb2grayscale_io_maybe.loadImage) files

               -- putStrLn $ show $ check_pattern [1,0,1,0]
             --   putStrLn $ show $ checker_pattern_areas 2482 3507 5 5
             --   putStrLn $ show $ detect_bookmarks_maybe images
                --putStrLn ""
                putStrLn $ show $ detect_bookmarks_maybe images
                --putStrLn $ show $ files
  where

    tag_DMap' = tag_DMap args

----------------------------------------------------------------------------------------------------


main = do

    getArgs >>= routine
    --putStr ""
    --test1


-----   -static -optl-static -optl-pthread
