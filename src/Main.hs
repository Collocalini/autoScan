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
---end of imports from this project




{-- ================================================================================================
================================================================================================ --}
routine:: [String] -> IO ()
routine args
  |otherwise = return ()
  where

    tag_DMap' = tag_DMap args

----------------------------------------------------------------------------------------------------


main = do

    getArgs >>= routine
    --putStr ""
    --test1


-----   -static -optl-static -optl-pthread
