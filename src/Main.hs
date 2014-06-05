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

import           Control.Applicative()
import           Control.Concurrent
import           Control.Concurrent.MVar()
import           Control.Exception
import           Snap.Core
import           Snap.Util.FileServe()
import           Data.List
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

--data AppState = AppState {share_mount :: FilePath
--                         ,in_work :: [(ThreadId, FilePath)]}

type AppState = (FilePath
                , [(ThreadId, FilePath)])
--type AppState1 = MVar (FilePath
--                , [(ThreadId, FilePath)])

main :: IO ()
main = do
   args <- getArgs

   case (hasPerform $ list_arguments args) of
     (True) -> autoScan args
     (False) -> do
       sm <- readFile "./share_mount"

       --quickHttpServe $ evalStateT site (AppState {share_mount = filter (/= '\n') sm
       --                                           , in_work = []})
       --quickHttpServe $ evalStateT site (filter (/= '\n') sm, [])


       id <- myThreadId
       as <- newMVar (filter (/= '\n') sm, [(id, filter (/= '\n') sm)])
       --as <- evalMState True site' (filter (/= '\n') sm, [(id, filter (/= '\n') sm)])
       --evalMState True site' (filter (/= '\n') sm, [(id, filter (/= '\n') sm)]) --) >>=
        --  quickHttpServe
       quickHttpServe $ site as
       return ()





{-- ================================================================================================
================================================================================================ --}
hasPerform :: [Maybe (String, String)] -> Bool
hasPerform [] = False
hasPerform (Nothing:_) = False
hasPerform (Just (p,_):_) = p == argument_perform
----------------------------------------------------------------------------------------------------


{--
{-- ================================================================================================
================================================================================================ --}
site' :: MState AppState IO (MState AppState Snap ())
--site' :: MVar [Int] -> Snap ()
site' =
    route [ (B.pack "test", testHandler)]

----------------------------------------------------------------------------------------------------
--}




{-- ================================================================================================
================================================================================================ --}
site :: MVar AppState ->  Snap ()
site appState = --do
  --  liftIO $ putStrLn $ "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    --ifTop (indexHandler)
    --runAutoScan
     -- <|>
    route [ (B.pack "/", indexHandler)
          , (B.pack "demo_a", demo_aHandler appState)
          , (B.pack "demo_b", demo_bHandler appState)
          , (B.pack "mogrify", mogrifyHandler appState)
          , (B.pack "isItReadyYet", isItReadyYetHandler appState)
          , (B.pack "kill", killHandler appState)
         -- , (B.pack "test", testHandler)
          --, (B.pack "submitted", writeBS $ B.pack "Please wait")

          ]
   -- <|>
   -- dir (B.pack "static") (serveDirectory "./static")


----------------------------------------------------------------------------------------------------

{-- ================================================================================================
================================================================================================ --}

indexHandler :: Snap ()
indexHandler = do
    index <- liftIO $ read_file_if_exists "./static/select_folder.html"
    writeBS $ B8.fromString index
-----------------------------------------------------------------------------------------------------



{-- ================================================================================================
================================================================================================ --}

readUserDir :: Maybe [B8.ByteString] -> FilePath
readUserDir Nothing = ""
readUserDir (Just p) =  (map step1 $ B8.toString $ B.concat p)
  where
  step1 :: Char -> Char
  step1 c
     |c == '\\' = '/'
     |otherwise = c
--B.unpack
-----------------------------------------------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
create_ls_file :: FilePath -> IO ()
create_ls_file cd = do
  path <- canonicalizePath $ cd ++ "/.."
  writeFile (cd ++ "/ls_script") $ "find \'" ++  path ++ "\' -maxdepth 1 -iname \"*.png\" -iname \"*.PNG\"|sort|tee \'" ++ cd ++ "/files\'"
-----------------------------------------------------------------------------------------------------




{-- ================================================================================================
================================================================================================ --}
create_mogrify_file :: FilePath -> IO ()
create_mogrify_file path = do
  --path <- canonicalizePath $ cd ++ "/.."
  writeFile (path ++ "/pdf_to_png") $ "cd \'" ++ path ++ "\' \n" ++ "mogrify -type TrueColor -colorspace RGB -format PNG *.pdf"

{--
autoScan_fork :: IO ()
              -> (Either SomeException ThreadId -> StateT AppState IO ())
              -> IO ThreadId
autoScan_fork action and_then = do

   mask $ \restore ->
      forkIO $ try (restore action) >>= (\t -> do
                                              at <- return $ and_then t
                                              return ())
  --return $ return ()
 -- where
 {--
  at ::   (Either SomeException ThreadId -> StateT AppState IO ())
        -> (Either SomeException ThreadId -> IO ())
  at and_then = do
     --a <- and_then
     --b <- a
     return and_then
--}

--}

autoScan_thread_is_dead :: MVar AppState -> Either SomeException () -> IO ()
autoScan_thread_is_dead appState e = case e of
                               (Right _) -> return ()
                               (Left  _) -> l
  where
  l = do
       id <- myThreadId

       (modifyMVar appState $ step2 id) >>= \(s', i') -> putStrLn $ show $ (s', i')


       where
       eqid :: ThreadId -> (ThreadId, FilePath) -> Bool
       eqid id (idn, _)
         |id /= idn = True
         |otherwise = False


       step2 :: ThreadId -> AppState -> IO (AppState, AppState)
       step2 id (share_mount',in_work') = do
           return (r, r)
           where
           r = (share_mount',filter (eqid id) in_work')
--}

--appState___in_work :: AppState -> [(ThreadId, FilePath)] -> AppState
--appState___in_work a f = a {in_work = f}


--appState___in_work :: AppState -> [(ThreadId, FilePath)] -> AppState
--appState___in_work (x,_) f = (x, f)

--}
{--
{-- ================================================================================================
================================================================================================ --}
testHandler :: MState AppState Snap ()
--testHandler :: MVar [Int] -> Snap ()
testHandler = do
    x<- get


   --writeBS $ B.pack $ show x1 ++ "\n"

   --x <- liftIO $ step2 x

   --x2<- liftIO $ modifyMVar x step3

   --put x2

   --x3 <- get

   --x <- liftIO $ modifyMVar mvar step1

   --(f,l) <- x
    liftIO $ putStrLn $ show x
   --writeBS $ B.pack $ show (f,l)

 {--  where
   step1 :: AppState -> IO (AppState, AppState)
--   step1 :: [Int] -> IO ([Int], [Int])
--   step1 y = let y'=head y+1 in return (y':y,y':y)
   step1 as@(share_mount',in_work') = do
       id <- myThreadId
       return ((share_mount',(id, "aaa"):in_work'), (share_mount',(id, "aaa"):in_work'))--}
 {--  step2 :: AppState -> IO AppState
   step2 as@(share_mount',in_work') = do
       id <- myThreadId
       return (share_mount',(id, "aaa"):in_work')

   step3 :: AppState1 -> IO (AppState1, AppState1)
--   step1 :: [Int] -> IO ([Int], [Int])
--   step1 y = let y'=head y+1 in return (y':y,y':y)
   step3 as = do
       (share_mount',in_work') <- readMVar as
       id <- myThreadId
       l <- newMVar (share_mount',(id, "aaa"):in_work')
       r<- newMVar (share_mount',(id, "aaa"):in_work')
       return (l, r)--}
 {--
   step4 :: AppState1 -> (AppState1 -> IO (AppState1, AppState1)) -> IO AppState1
   step4 a fn = modifyMVar a step3--}
 -----------------------------------------------------------------------------------------------------
--}



{-- ================================================================================================
================================================================================================ --}
killHandler :: MVar AppState -> Snap ()
killHandler appState = do

   (share_mount', in_work') <- liftIO $ readMVar appState

   cd <- getsRequest (rqParam  $ B.pack "cd")
   let cd' = share_mount' ++ "/" ++ (readUserDir cd)
   path <- liftIO $ canonicalizePath $ cd' ++ "/.."

   liftIO $ putStrLn $ show $ in_work'

   let (l,r) = partition (eqpath path) in_work'

   _ <- liftIO $ mapM kill l

   (liftIO $ modifyMVar appState $ step2 r) >>= \(s', i') -> liftIO $ putStrLn $ show $ (s', i')

   indexHandler
   where
   eqpath :: FilePath -> (ThreadId, FilePath) -> Bool
   eqpath path (_, pathn)
     |path == pathn = True
     |otherwise = False
   kill :: (ThreadId, FilePath) -> IO ()
   kill (id, _) = killThread id

   --step2 :: -> AppState -> IO (AppState, AppState)
   step2 list (share_mount',_) = do
       return (r, r)
       where
       r = (share_mount', list)

-----------------------------------------------------------------------------------------------------





{-- ================================================================================================
================================================================================================ --}
isItReadyYetHandler :: MVar AppState -> Snap ()
isItReadyYetHandler appState = do

   (share_mount', in_work') <- liftIO $ readMVar appState

   cd <- getsRequest (rqParam  $ B.pack "cd")
   let cd' = share_mount' ++ "/" ++ (readUserDir cd)
   path <- liftIO $ canonicalizePath $ cd' ++ "/.."

   liftIO $ putStrLn $ show $ in_work'

   case (n_o_t_in_use in_work' path) of
     (True) -> do
        index <- liftIO $ read_file_if_exists "./static/done.html"
        writeBS $ B.pack index
     (False) -> do
        submitt cd

-----------------------------------------------------------------------------------------------------



submitt :: Maybe [B.ByteString] -> Snap ()
submitt cd = do
  index <- liftIO $ read_file_if_exists "./static/submitted.html"
  writeBS $ B.pack $ index ++ " \n <input title=\"path to folder\" id=\"path\" name=\"path\" "
          ++ "type=\"text\" hidden=\"true\" value=\'" ++ ( B.unpack $ mbStrToStr cd)
          ++ "\' />   </body></html>"




{-- ================================================================================================
================================================================================================ --}
mogrifyHandler :: MVar AppState -> Snap ()
mogrifyHandler appState = do

   (share_mount', in_work') <- liftIO $ readMVar appState

   cd <- getsRequest (rqParam  $ B.pack "cd")
   let cd' = share_mount' ++ "/" ++ (readUserDir cd)
   path <- liftIO $ canonicalizePath $ cd' ++ "/.."

   liftIO $ putStrLn $ show $ in_work'

   case (n_o_t_in_use in_work' path) of
     (True) -> do
        liftIO $ create_mogrify_file path

        _ <- liftIO $ forkFinally (wrapAutoScan appState path $ run_script (path ++ "/pdf_to_png"))
                                  (autoScan_thread_is_dead appState)

        submitt cd
        --writeBS $ B.pack "please wait"
     (False) -> writeBS $ B.pack "directory is in use"

-----------------------------------------------------------------------------------------------------


mbStrToStr :: Maybe [B.ByteString] -> B.ByteString
mbStrToStr Nothing  = B.pack ""
mbStrToStr (Just s) = B.concat s


{-- ================================================================================================
================================================================================================ --}
n_o_t_in_use :: [(ThreadId, FilePath)] -> FilePath -> Bool
n_o_t_in_use [] _ = True
n_o_t_in_use ((_,f):rest) p
  | p/=f      = n_o_t_in_use rest p
  |otherwise = False
-----------------------------------------------------------------------------------------------------





{-- ================================================================================================
================================================================================================ --}
wrapAutoScan :: MVar AppState -> FilePath ->  IO () -> IO ()
wrapAutoScan appState path as = do

   id <- myThreadId

   (modifyMVar appState $ step1 id ) >>= \(s', i') -> putStrLn $ show $ (s', i')

   as

   (modifyMVar appState $ step2 id ) >>= \(s', i') -> putStrLn $ show $ (s', i')


   where
   eqid :: ThreadId -> (ThreadId, FilePath) -> Bool
   eqid id' (idn, _)
     |id' /= idn = True
     |otherwise = False

   step1 :: ThreadId -> AppState -> IO (AppState, AppState)
   step1 id' (share_mount',in_work') = do
       return (r, r)
       where
       r = (share_mount',(id', path):in_work')

   step2 :: ThreadId -> AppState -> IO (AppState, AppState)
   step2 id' (share_mount',in_work') = do
       return (r, r)
       where
       r = (share_mount',filter (eqid id') in_work')
-----------------------------------------------------------------------------------------------------





{-- ================================================================================================
================================================================================================ --}
demoHandler_common :: MVar AppState ->
   Snap (Maybe [B.ByteString], FilePath, FilePath, String, FilePath, FilePath)
demoHandler_common appState = do
   (share_mount', in_work') <- liftIO $ readMVar appState

   cd <- getsRequest (rqParam  $ B.pack "cd")
   af <- getsRequest (rqParam $ B.pack "af")
   wpt <- getsRequest (rqParam $ B.pack "wpt")

   let cd' = share_mount' ++ "/" ++ (readUserDir cd)
   path_cd <- liftIO $ canonicalizePath $ cd' ++ "/.."
   cd' <- liftIO $ canonicalizePath cd'
   let wpt' = (\s -> case s of
                        (Nothing) -> ""
                        (Just s')  -> B8.toString $ B.concat s') wpt

   let af' = readUserDir af
   let path_af = af' -- <- liftIO $ canonicalizePath af'

   liftIO $ putStrLn $ show $ in_work'
   liftIO $ putStrLn wpt'
   return (cd, cd', af', wpt', path_cd, path_af)
-----------------------------------------------------------------------------------------------------






{-- ================================================================================================
================================================================================================ --}
demo_aHandler :: MVar AppState -> Snap ()
demo_aHandler appState = do
   (_, in_work') <- liftIO $ readMVar appState
   (cd, cd', af', wpt,  path_cd, _) <- demoHandler_common appState

   case (n_o_t_in_use in_work' path_cd) of
     (True) -> do
        liftIO $ create_ls_file cd'

        _ <- liftIO $ forkFinally (wrapAutoScan appState path_cd $ runAutoScan_a cd' af' wpt)
                                  (autoScan_thread_is_dead appState)

        submitt cd
     (False) -> writeBS $ B.pack "directory is in use"
-----------------------------------------------------------------------------------------------------








{--
wrapAutoScan :: IO () -> IO ThreadId
wrapAutoScan as = do
   liftIO as
   myThreadId
--}




{-- ================================================================================================
================================================================================================ --}
runAutoScan_a :: FilePath -> FilePath -> String ->  IO ()
runAutoScan_a cd af wpt = do
   autoScan $ ["--perform"] ++ ["analyse,save-analysis-results,use-analysis-results"] ++
          ["--analysis-results-file"]      ++ [cd ++ "/" ++ af] ++
          ["--analysis-detected-pages-to"] ++ [ cd ++ "/d"]  ++
          ["--analysis-white-pages-to"]    ++ [ cd ++ "/w"]  ++
          ["--white-pages-to-pdf"]         ++ [ cd ++ "/zw"] ++
          ["--detected-pages-to-pdf"]      ++ [ cd ++ "/zd"] ++
          ["--scans-to-pdfs"]              ++ [ cd ++ "/zp"] ++
          ["--working-folder"]             ++ [cd]          ++
          ["--scripts-folder"]             ++ [cd]          ++
          ["--white-page-tolerance"]       ++ [wpt]
-----------------------------------------------------------------------------------------------------





{-- ================================================================================================
================================================================================================ --}
demo_bHandler :: MVar AppState -> Snap ()
demo_bHandler appState = do
   (_, in_work') <- liftIO $ readMVar appState
   (cd, cd', af', _,  path_cd, _) <- demoHandler_common appState

   case (n_o_t_in_use in_work' path_cd) of
     (True) -> do
        liftIO $ create_ls_file cd'

        _ <- liftIO $ forkFinally (wrapAutoScan appState path_cd $ runAutoScan_b cd' af')
                                  (autoScan_thread_is_dead appState)

        submitt cd
     (False) -> writeBS $ B.pack "directory is in use"
-----------------------------------------------------------------------------------------------------







{-- ================================================================================================
================================================================================================ --}
runAutoScan_b :: FilePath -> FilePath -> IO ()
runAutoScan_b cd af = do
   autoScan $ ["--perform"] ++ ["read-analysis,use-analysis-results"] ++
          ["--analysis-results-file"]      ++ [cd ++ "/" ++ af] ++
          ["--analysis-detected-pages-to"] ++ [ cd ++ "/d"]  ++
          ["--analysis-white-pages-to"]    ++ [ cd ++ "/w"]  ++
          ["--white-pages-to-pdf"]         ++ [ cd ++ "/zw"] ++
          ["--detected-pages-to-pdf"]      ++ [ cd ++ "/zd"] ++
          ["--scans-to-pdfs"]              ++ [ cd ++ "/zp"] ++
          ["--working-folder"]             ++ [cd]          ++
          ["--scripts-folder"]             ++ [cd]        --  ++
          --["--white-page-tolerance"]       ++ [" 100 "]
-----------------------------------------------------------------------------------------------------




--}



