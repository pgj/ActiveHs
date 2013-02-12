{-# LANGUAGE ViewPatterns, PatternGuards, OverloadedStrings #-}
module Logger
    ( Logger
    , newLogger
    , logMsg
    , logStrMsg
    ) where

import qualified System.FastLogger as SF

import Control.Applicative
import Control.Monad

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B

-------


data Logger = Logger
    { logger :: SF.Logger
    , level  :: Int
        -- 0: very important message, usually an error report, wrapped in '#####'
        -- 1: important message, wrapped in '####'
        -- 2: normal message
        -- 3, 4, ...: debug messages (not shown by default)
    }

newLogger :: Int -> FilePath -> IO Logger
newLogger l path
    = Logger <$> SF.newLogger path <*> pure l

logStrMsg :: Int -> Logger -> String -> IO ()
logStrMsg l lg
    = logMsg l lg . B.fromString

logMsg :: Int -> Logger -> B.ByteString -> IO ()
logMsg l lg msg = do
    msg' <- SF.timestampedLogEntry $ highlight l msg
    when (l <= level lg) $ SF.logMsg (logger lg) msg'
  where
    highlight 0 = wrap "#####"
    highlight 1 = wrap "####"
    highlight _ = id

    wrap n x = B.concat [n, "    ", x, "    ", n]

