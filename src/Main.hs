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
import           Snap.Util.FileServe()
--import           Snap.Util.FileUploads
import           Snap.Http.Server
import qualified Data.ByteString.Char8 as B
import "monads-tf" Control.Monad.Reader
import System.Environment
import System.Directory


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
    ifTop (indexHandler) <|>
    route [ (B.pack "demo_a", demo_aHandler)
          , (B.pack "demo_b", demo_bHandler)
          , (B.pack "submitted", writeBS $ B.pack "Please wait")

          ]-- <|>
   -- dir (B.pack "static") (serveDirectory "./static")
----------------------------------------------------------------------------------------------------


indexHandler :: Snap ()
indexHandler = do
    index <- liftIO $ read_file_if_exists "./static/select_folder.html"
    writeBS $ B.pack index


  {--
maxMb = 2
megaByte = 2^(20::Int)

myDefaultPolicy :: UploadPolicy
myDefaultPolicy = setMaximumFormInputSize (maxMb * megaByte) defaultUploadPolicy

myPerPartPolicy :: PartInfo -> PartUploadPolicy
myPerPartPolicy _ = allowWithMaximumSize (maxMb * megaByte)


myBusinessLogic :: MonadSnap m => PartInfo -> FilePath -> m ()
myBusinessLogic _ p = do
   index <- liftIO $ read_file_if_exists p
   liftIO $ putStrLn $ "index=" ++ index
   --writeBS $ B.pack index
   --undefined -- whatever you actually want to do with your uploads


myUploadHandler :: MonadSnap m =>
                [(PartInfo, Either PolicyViolationException FilePath)] -> m ()
myUploadHandler xs = mapM_ handleOne (filter wanted xs) where
 wanted (_,Left _) = False
 wanted (_,Right _) = True
 handleOne (partInfo,Right filepath) = myBusinessLogic partInfo filepath


myHandleFileUploads :: MonadSnap m => FilePath -> m ()
myHandleFileUploads tempDir =
   handleFileUploads tempDir defaultUploadPolicy  myPerPartPolicy myUploadHandler


--}
{--uploadFiles :: AppHandler ()
uploadFiles = do
  files <- handleMultipart defaultUploadPolicy $ \part -> do
    content <-  liftM B.concat EL.consume
    return (part, content)
  if any (\(_,c) -> (B.length c) > 16000000) files
    then error "One of the uploaded files is bigger then 16Mb limit size!"
    else saveFiles files
  syncDB
  redirect "/files/all"--}



{-- ================================================================================================
================================================================================================ --}
echoHandler :: Snap ()
echoHandler = do
    param <- getParam $ B.pack "echoparam"
    maybe (writeBS $ B.pack "must specify echo/param in URL")
          writeBS param
----------------------------------------------------------------------------------------------------




readUserDir :: Maybe [B.ByteString] -> FilePath
readUserDir Nothing = ""
readUserDir (Just p) =  B.unpack $ B.concat p




{-- ================================================================================================
================================================================================================ --}
demo_aHandler :: Snap ()
demo_aHandler = do
   --writeBS $ B.pack "Demo autoScan is running"
   --d <- liftIO $ getAppUserDataDirectory "hokum"
   --liftIO $ putStrLn d
   cd <- getsRequest (rqParam $ B.pack "cd")
   liftIO $ putStrLn $ show cd
   --myHandleFileUploads "."
   pwd <- liftIO getCurrentDirectory

   liftIO $ setCurrentDirectory $ readUserDir cd
   liftIO $ autoScan $ words $ " --perform analyse,save-analysis-results,use-analysis-results " ++
    "--analysis-results-file      ./a    " ++
    "--analysis-detected-pages-to ./d    " ++
    "--analysis-white-pages-to    ./w    " ++
    "--white-pages-to-pdf         ./zw   " ++
    "--detected-pages-to-pdf      ./zd   " ++
    "--scans-to-pdfs              ./zp   " ++
    "--white-page-tolerance       100    " ++
    "--scripts-folder             " ++ pwd
   --}
   liftIO $ setCurrentDirectory pwd

   --indexHandler
   --writeBS $ B.pack "Demo autoScan is done"
-----------------------------------------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
demo_bHandler :: Snap ()
demo_bHandler = do

   cd <- getsRequest (rqParam $ B.pack "cd")
   af <- getsRequest (rqParam $ B.pack "af")

   liftIO $ putStrLn $ show cd
   liftIO $ putStrLn $ show af
   pwd <- liftIO getCurrentDirectory

   liftIO $ setCurrentDirectory $ readUserDir cd
   liftIO $ autoScan $ words $ " --perform read-analysis,use-analysis-results " ++
    "--analysis-results-file      ./" ++ readUserDir af  ++ "    " ++
    "--analysis-detected-pages-to ./d    " ++
    "--analysis-white-pages-to    ./w    " ++
    "--white-pages-to-pdf         ./zw   " ++
    "--detected-pages-to-pdf      ./zd   " ++
    "--scans-to-pdfs              ./zp   " ++
    "--white-page-tolerance       100    "

   --}
   liftIO $ setCurrentDirectory pwd

   --indexHandler
   --writeBS $ B.pack "Demo autoScan is done"
-----------------------------------------------------------------------------------------------------


