{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, ViewPatterns, PatternGuards, NamedFieldPuns #-}

module Main where

import Smart
import Cache
import Converter
import Args
import Special
import Lang
import Logger
import Result
import Html
import Hash

import Snap.Core
import Snap.Http.Server (httpServe)
import Snap.Http.Server.Config
import Snap.Util.FileServe (getSafePath, serveDirectoryWith, simpleDirectoryConfig)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)

import System.FilePath ((</>), takeExtension, dropExtension)
import System.Directory (doesFileExist)
--import Language.Haskell.Interpreter (liftIO)
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Applicative ((<|>))
import System.Locale (defaultTimeLocale)
import Data.Time (getCurrentTime, formatTime, diffUTCTime)
import Data.Maybe (listToMaybe)
--import Prelude hiding (catch)

---------------------------------------------------------------

main :: IO ()
main = getArgs >>= mainWithArgs

mainWithArgs :: Args -> IO ()
mainWithArgs args@(Args {verbose, port, static, logdir, hoogledb, fileservedir, gendir, mainpage, restartpath, sourcedir, includedir}) = do 

    ti <- getCurrentTime
    log <- newLogger verbose $ 
        logdir </> "interpreter_" ++ formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S" ti ++ ".log"
             -- "%Y-%m-%d-%H:%M:%S" is not ok, colons are not supported in filenames under windows

    ch <- startGHCiServer [sourcedir] log hoogledb
    cache <- newCache 10

    httpServe

        ( setPort port
        . setAccessLog (if null logdir then ConfigNoLog else ConfigFileLog (logdir </> "access.log"))
        . setErrorLog  (if null logdir then ConfigNoLog else ConfigFileLog (logdir </> "error.log"))
        $ emptyConfig
        )

        (   method GET
                (   serveDirectoryWith simpleDirectoryConfig fileservedir
                <|> serveHtml ch
                <|> ifTop (redirect $ fromString mainpage)
                <|> path (fromString restartpath) (liftIO $ restart ch >> clearCache cache)
                )
        <|> method POST (exerciseServer (sourcedir:includedir) (cache, ch) args)
        <|> notFound
        )
  where
    serveHtml ch = do
        p <- getSafePath
        when (not static && takeExtension p `elem` [".xml"]) $ liftIO $
            convert ch args $ dropExtension p
        serveDirectoryWith simpleDirectoryConfig gendir

    notFound :: Snap ()
    notFound = do
        writeText "<html xmlns=\"http://www.w3.org/1999/xhtml\"><body>Page not found.</body></html>"
        getResponse >>= finishWith . setResponseCode 404






---------------------------------------------------------------

getParam' :: ByteString -> Snap (Maybe T.Text)
getParam' = fmap (fmap $ decodeUtf8With lenientDecode) . getParam

type TaskChan' = (Cache (Int, T.Text), TaskChan)

exerciseServer :: [FilePath] -> TaskChan' -> Args -> Snap ()
exerciseServer sourcedirs (cache, ch) args@(Args {magicname, lang, exercisedir, verboseinterpreter}) = do
    params <- fmap show getParams
    when (length params > 3000) $ do
        writeText "<html xmlns=\"http://www.w3.org/1999/xhtml\"><body>Too long request.</body></html>"
        getResponse >>= finishWith . setResponseCode 400

    let md5Id = mkHash params   -- could be more efficient
    liftIO $ logStrMsg 2 (logger ch) $ " eval " ++ show md5Id ++ " " ++ params
    j <- liftIO $ lookupCache cache md5Id
    (>>= writeText) $ case j of
      Left (delay, res) -> liftIO $ do
        logStrMsg 2 (logger ch) $ "   ch " ++ show md5Id
        threadDelay delay
        return res
      Right cacheAction -> do
        time <- liftIO $ getCurrentTime
        res <- fmap renderResults $ do
            Just [ss, fn_, x, y, T.unpack -> lang']  <- fmap sequence $ mapM getParam' ["c","f","x","y","lang"]

            let fn = exercisedir </> T.unpack fn_
                ext = reverse $ takeWhile (/='.') $ reverse fn
            True <- liftIO $ doesFileExist fn
            Just task <- liftIO $ fmap (eval_ ext ss y . T.splitOn (T.pack delim)) $ T.readFile fn
            liftIO $ exerciseServer' ('X':magicname) ch verboseinterpreter fn x lang md5Id task

         <|> return [Error True $ translate lang "Inconsistency between server and client."]

        liftIO $ do
            time' <- getCurrentTime
            let delay = round $ 1000000 * (realToFrac $ diffUTCTime time' time :: Double) :: Int
            logStrMsg 2 (logger ch) $ "  end " ++ show md5Id ++ " " ++ show delay ++ " ms  " ++ T.unpack res
            cacheAction (delay, res)
            return res

  where
    eval_ _ "eval"  _ [_]
        = Just Eval
    eval_ _ "eval"  _ [_, goodsol]
        = Just $ Compare magicname $ T.unpack $ T.drop (length magicname + 4) $ goodsol
    eval_ ext comm
      (T.unpack -> s) 
      [env, hidden, re -> Just (is :: [([String],String)]), T.unpack -> j, T.unpack -> i, re -> Just funnames] 
        = Just $ case comm of 
            "eval2" -> Compare2 env funnames s
            "check" -> Check ext sourcedirs env funnames is i j
    eval_ _ _ _ _
        = Nothing

maybeRead :: Read a => String -> Maybe a
maybeRead s = listToMaybe [a | (a,"") <- reads s] 

re :: Read b => T.Text -> Maybe b
re = maybeRead . T.unpack


