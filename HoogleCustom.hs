{-# LANGUAGE OverloadedStrings #-}
module HoogleCustom
    ( query
    , queryInfo
    ) where

import Result
import Lang

import qualified Hoogle as H

-------------------------

query :: FilePath -> String -> IO Result
query ch q = format <$> search' ch q

queryInfo :: Language -> FilePath -> String -> IO Result
queryInfo lang db q = do
    res <- search' db q
    case res of
      (r : _) -> return $ SearchResults False [H.targetDocs r]
      []      -> return $ Message (translate lang "No info for " ++ q) Nothing

search' :: FilePath -> String -> IO [H.Target]
search' dbname q = H.withDatabase dbname
                     (\db -> return $ H.searchDatabase db q)

format :: [H.Target] -> Result
format r = SearchResults (not $ null b) (map H.targetDocs a)
  where
    (a, b) = splitAt 10 r
