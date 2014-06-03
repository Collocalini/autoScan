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
import "monads-tf"  Control.Monad.State
import           Control.Concurrent
import           Snap.Core
import           Snap.Util.FileServe()
--import           Snap.Util.FileUploads
import           Snap.Http.Server
import qualified Data.ByteString.UTF8 as B8
import qualified Data.ByteString.Char8 as B
import "monads-tf" Control.Monad.Reader
import System.Environment
import System.Directory


-- from this project
import AutoScan
import Global
-- end of imports from this project



--type AppState = (Bool, FilePath, FilePath)

data AppState = AppState {share_mount :: FilePath}

main :: IO ()
main = do
   args <- getArgs

   case (hasPerform $ list_arguments args) of
     (True) -> autoScan args
     (False) -> do
       sm <- readFile "./share_mount"

       quickHttpServe $ evalStateT site (AppState {share_mount = filter (/= '\n') sm})





{-- ================================================================================================
================================================================================================ --}
hasPerform :: [Maybe (String, String)] -> Bool
hasPerform [] = False
hasPerform (Nothing:_) = False
hasPerform (Just (p,_):_) = p == argument_perform
----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}
site :: StateT AppState Snap ()
site = --do
  --  liftIO $ putStrLn $ "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    --ifTop (indexHandler)
    --runAutoScan
     -- <|>
    route [ (B.pack "/", lift indexHandler)
          , (B.pack "demo_a", demo_aHandler)
          , (B.pack "demo_b", demo_bHandler)
          , (B.pack "mogrify", mogrifyHandler)
          --, (B.pack "submitted", writeBS $ B.pack "Please wait")

          ]
   -- <|>
   -- dir (B.pack "static") (serveDirectory "./static")


----------------------------------------------------------------------------------------------------


indexHandler :: Snap ()
indexHandler = do
    index <- liftIO $ read_file_if_exists "./static/select_folder.html"
    writeBS $ B.pack index


readUserDir :: Maybe [B8.ByteString] -> FilePath
readUserDir Nothing = ""
readUserDir (Just p) =  (map step1 $ B8.toString $ B.concat p)
  where
  step1 :: Char -> Char
  step1 c
     |c == '\\' = '/'
     |otherwise = c
--B.unpack



create_ls_file cd = do
  path <- canonicalizePath $ cd ++ "/.."
  writeFile (cd ++ "/ls_script") $ "find \'" ++  path ++ "\' -maxdepth 1 -iname \"*.png\" -iname \"*.PNG\"|sort|tee \'" ++ cd ++ "/files\'"



create_mogrify_file path = do
  --path <- canonicalizePath $ cd ++ "/.."
  writeFile (path ++ "/pdf_to_png") $ "cd \'" ++ path ++ "\' \n" ++ "mogrify -format PNG *.pdf"



{-- ================================================================================================
================================================================================================ --}
mogrifyHandler :: StateT AppState Snap ()
mogrifyHandler = do
   (AppState {share_mount = share_mount'}) <- get

   cd <- getsRequest (rqParam  $ B.pack "cd")
   let cd' = share_mount' ++ "/" ++ (readUserDir cd)
   path <- liftIO $ canonicalizePath $ cd' ++ "/.."

   liftIO $ create_mogrify_file path
   _ <- liftIO $ forkIO $ run_script (path ++ "/pdf_to_png")

   writeBS $ B.pack "please wait"
-----------------------------------------------------------------------------------------------------





{-- ================================================================================================
================================================================================================ --}
demo_aHandler :: StateT AppState  Snap ()
demo_aHandler = do
   (AppState {share_mount = share_mount'}) <- get
   cd <- getsRequest (rqParam  $ B.pack "cd")
   af <- getsRequest (rqParam $ B.pack "af")

   let cd' = share_mount' ++ "/" ++ (readUserDir cd)
   let af' = readUserDir af

   liftIO $ create_ls_file cd'

   _ <- liftIO $ forkIO $ runAutoScan_a cd' af'

   writeBS $ B.pack "please wait"


-----------------------------------------------------------------------------------------------------




runAutoScan_a cd af = do
   autoScan $ ["--perform"] ++ ["analyse,save-analysis-results,use-analysis-results"] ++
          ["--analysis-results-file"]      ++ [ cd ++ "/" ++ af ++ ""] ++
          ["--analysis-detected-pages-to"] ++ [ cd ++ "/d"]  ++
          ["--analysis-white-pages-to"]    ++ [ cd ++ "/w"]  ++
          ["--white-pages-to-pdf"]         ++ [ cd ++ "/zw"] ++
          ["--detected-pages-to-pdf"]      ++ [ cd ++ "/zd"] ++
          ["--scans-to-pdfs"]              ++ [ cd ++ "/zp"] ++
          ["--working-folder"]             ++ [cd]          ++
          ["--scripts-folder"]             ++ [cd]          ++
          ["--white-page-tolerance"]       ++ [" 100 "]




{-- ================================================================================================
================================================================================================ --}
demo_bHandler :: StateT AppState Snap ()
demo_bHandler = do
   (AppState {share_mount = share_mount'}) <- get
   cd <- getsRequest (rqParam  $ B.pack "cd")
   af <- getsRequest (rqParam $ B.pack "af")

   let cd' = share_mount' ++ "/" ++ (readUserDir cd)
   let af' = readUserDir af

   _ <- liftIO $ forkIO $ runAutoScan_b cd' af'

   writeBS $ B.pack "please wait"


-----------------------------------------------------------------------------------------------------







--runAutoScan :: IO ()
runAutoScan_b cd af = do
   autoScan $ ["--perform"] ++ ["read-analysis,use-analysis-results"] ++
          ["--analysis-results-file"]      ++ [ cd ++ "/" ++ af ++ ""] ++
          ["--analysis-detected-pages-to"] ++ [ cd ++ "/d"]  ++
          ["--analysis-white-pages-to"]    ++ [ cd ++ "/w"]  ++
          ["--white-pages-to-pdf"]         ++ [ cd ++ "/zw"] ++
          ["--detected-pages-to-pdf"]      ++ [ cd ++ "/zd"] ++
          ["--scans-to-pdfs"]              ++ [ cd ++ "/zp"] ++
          ["--working-folder"]             ++ [cd]          ++
          ["--scripts-folder"]             ++ [cd]          ++
          ["--white-page-tolerance"]       ++ [" 100 "]







