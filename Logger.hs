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
    , level  :: Int         -- 0: most important
    }

newLogger l path
    = Logger <$> SF.newLogger path <*> pure l

logStrMsg l lg
    = logMsg l lg . B.fromString

logMsg l lg msg = do
    msg' <- SF.timestampedLogEntry msg
    when (l <= level lg) $ SF.logMsg (logger lg) msg'


