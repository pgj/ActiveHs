module QuickCheck where

import Smart
import ActiveHs.Base (WrapData2 (WrapData2), TestCase (TestCase))
import Lang
import Result
import Logger
import Qualify (qualify)
import Hash

import Test.QuickCheck hiding (Result)
import qualified Test.QuickCheck.Property as QC

import Data.Char (isLower)
import Data.List (intercalate)
import Control.Monad (join)
import Control.Concurrent.MVar

---------------------------------------

quickCheck
    :: String
    -> Hash
    -> Language
    -> TaskChan
    -> FilePath
    -> String
    -> [String]
    -> [([String],String)]      -- test cases
    -> IO [Result]
quickCheck qualifier m5 lang ch fn ft funnames testcases = do
    logStrMsg 3 (logger ch) $ "test cases: " ++ show testcases
    logStrMsg 3 (logger ch) $ "names to be qualified: " ++ show funnames

    case allRight $ map (qualify qualifier funnames . snd) testcases of
        Left err -> do
            logStrMsg 3 (logger ch) $ "Error in qualification: " ++ err
            return [Error True err]
        Right s_ -> do
            logStrMsg 3 (logger ch) $ "Qualified expressions: " ++ show s_

            let ts = mkTestCases [(v,s,s') | ((v,s),s')<- zip testcases s_]
            logStrMsg 3 (logger ch) $ "Test cases: " ++ ts
            
            interp False m5 lang ch fn "" $ \a ->
                do  liftIO $ logStrMsg 3 (logger ch) "Before interpretation"
                    m <- interpret ts (as :: [TestCase])
                    liftIO $ logStrMsg 3 (logger ch) "After interpretation"
                    return $ qcs lang (logger ch) m

  where
    mkTestCases ss 
        = "[" ++ intercalate ", " (map mkTestCase ss) ++ "]"

    mkTestCase (vars, s, s2)  
        = "TestCase (\\qcinner " 
        ++ unwords ["(" ++ v ++ ")" | v<-vars] 
        ++ " -> qcinner (" ++ showTr vars s ++ ", " ++ parens s ++ ", " ++ parens s2 ++ "))"

    showTr vars s = "unwords [" ++ intercalate ", " (map g $ words s) ++ "]"  -- !!!
     where

        vs = concatMap f vars

        f = filter (isLower . head) . words

        g x | x `elem` vs = {- "\"(\" ++ -} " show " ++ x -- ++ " ++ \")\""
        g x = show x

qcs :: Language -> Logger -> [TestCase] -> IO [Result]
qcs lang log = join . foldM (return [Message (translate lang "All test cases are completed.") Nothing]) (qc lang log)

-- test = qc $ TestCase $ \f (QCNat x) (QCInt y) -> f (show x ++ " + " ++ show y, min 10 (x + y), x + y)
-- test' = qc $ TestCase $ \f (QCInt x) -> f ("sqr " ++ show x, min 10 (x*x), x*x)

qc :: Language -> Logger -> TestCase -> IO (Maybe (IO [Result]))
qc lang log (TestCase p) = do
    v <- newMVar Nothing
    let ff (s, x, y)
            = QC.whenFail (modifyMVar_ v $ const $ return $ Just $ fmap (ModifyCommandLine s :) res)
            $ QC.morallyDubiousIOProperty
            $ fmap (\s -> if hasError s then QC.failed else QC.succeeded) res
          where
            res = do
                logStrMsg 4 log $ "compare: " ++ s
                compareClearGen lang "noId" $ WrapData2 x y
    _ <- quickCheckWithResult (stdArgs { chatty = False }) $ {- QC.noShrinking $ -} p ff
    takeMVar v

------------------------------------

allRight :: [Either a b] -> Either a [b]
allRight x = case [s | Left s<-x] of
    (y:_) -> Left y
    []    -> Right [s | Right s<-x]

foldM :: Monad m => r -> (a -> m (Maybe r)) -> [a] -> m r
foldM end _ [] = return end
foldM end f (t:ts) = do
    x <- f t
    case x of
        Just r -> return r
        Nothing  -> foldM end f ts


