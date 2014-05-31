----------------------------------------------------------------------------
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

module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import qualified Data.ByteString.Char8 as B
import "monads-tf" Control.Monad.Reader
import System.Environment

-- from this project
import AutoScan
import Global
-- end of imports from this project

main :: IO ()
main = do
   args <- getArgs

   case (hasPerform $ list_arguments args) of
     (True) -> autoScan args
     (False) -> quickHttpServe site





{-- ================================================================================================
================================================================================================ --}
hasPerform :: [Maybe (String, String)] -> Bool
hasPerform [] = False
hasPerform (Nothing:_) = False
hasPerform (Just (p,_):_) = p == argument_perform
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
site :: Snap ()
site =
    ifTop (writeBS $ B.pack "hello world-----") <|>
    route [ (B.pack "demo", demoHandler)
          , (B.pack "echo/:echoparam", echoHandler)
          ] <|>
    dir (B.pack "static") (serveDirectory ".")
----------------------------------------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
echoHandler :: Snap ()
echoHandler = do
    param <- getParam $ B.pack "echoparam"
    maybe (writeBS $ B.pack "must specify echo/param in URL")
          writeBS param
----------------------------------------------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
demoHandler :: Snap ()
demoHandler = do
   writeBS $ B.pack "Demo autoScan is running"
   liftIO $ autoScan $ words $ " --perform analyse,save-analysis-results,use-analysis-results " ++
    "--analysis-results-file      ./a    " ++
    "--analysis-detected-pages-to ./d    " ++
    "--analysis-white-pages-to    ./w    " ++
    "--white-pages-to-pdf         ./zw   " ++
    "--detected-pages-to-pdf      ./zd   " ++
    "--scans-to-pdfs              ./zp   " ++
    "--white-page-tolerance       100"
   writeBS $ B.pack "Demo autoScan is done"
-----------------------------------------------------------------------------------------------------
