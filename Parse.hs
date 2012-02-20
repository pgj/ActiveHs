{-# LANGUAGE RelaxedPolyRec, PatternGuards, ViewPatterns #-}

module Parse 
    ( ParseMode (..)
    , Module (..)
    , Doc (..)
    , BBlock (..)
    , Name
    , Prompt
    , mainParse
    , getCommand
    , printName
    , parseQuickCheck
    ) where

import Text.Pandoc

import AgdaHighlight

import qualified Language.Haskell.Exts.Parser as HPar
import qualified Language.Haskell.Exts.Syntax as HSyn

{- Agda support (unfinished) -}
import qualified Agda.Syntax.Parser as APar
import qualified Agda.Syntax.Concrete as ASyn

import Data.List.Split (splitOn)
import Data.List (tails, partition, groupBy)
import Data.Function (on)
import Data.Char (isAlpha, isSpace, toUpper, isUpper)
import Control.Monad (zipWithM)

--------------------------------- data structures

data ParseMode = HaskellMode | AgdaMode
                 deriving (Show, Enum, Eq)

data Module
    = HaskellModule HSyn.Module
    | AgdaModule ASyn.Module
    deriving (Show)

data Doc
    = Doc
        Meta{-title, author, date-}
        Module{-module directives, module name, imports-}
        [BBlock]
        deriving (Show)

data BBlock
    = Text Block{-pandoc block-}
    | OneLineExercise
        Prompt
        Bool{-intentional error-}
        String
    | Exercise
        [String]{-lines-}
        [String]{-visible lines-}
        [String]{-hidden lines-}
        [Name]{-defined names-}
        [String]{-test expressions-}
        deriving (Show)

type Prompt = Char  -- see the separate documentation

type Name = String

-----------------------------------

commandList, testCommandList :: String
commandList = "AaRr" ++ testCommandList
testCommandList = "EeFfH"

-----------------------------------

mainParse :: ParseMode -> FilePath -> IO Doc
mainParse mode s = do
    c <- readFile s
    case readMarkdown pState . unlines . concatMap preprocess . lines $ c of
        Pandoc meta (CodeBlock ("",["sourceCode","literate","haskell"],[]) h: blocks) -> do
            header <- parseModule mode $ h
            fmap (Doc meta header) $ collectTests mode $ map (highlight . interpreter . Text) blocks
        Pandoc meta blocks -> do
            header <- parseModule mode $ "module Unknown where"
            fmap (Doc meta header) $ collectTests mode $ map (highlight . interpreter . Text) blocks
    where
        parseModule :: ParseMode -> String -> IO Module
        parseModule AgdaMode    m = fmap AgdaModule (APar.parse APar.moduleParser m)
        parseModule HaskellMode m = case HPar.parseModuleWithMode HPar.defaultParseMode m of
            (HPar.ParseOk m) -> return $ HaskellModule m
            parseError       -> fail $ "parseHeader: " ++ show parseError
        
        preprocess (c:'>':' ':l) | c `elem` commandList
            = ["~~~~~ {." ++ [c] ++ "}", dropWhile (==' ') l, "~~~~~", ""]
        preprocess ('|':l) 
            = []
        -- drop lines ending with "--"
        preprocess l | drop ((length l) - 2) l == "--" = []
                     | otherwise = [l]
        
        pState = defaultParserState 
            { stateSmart = True
            , stateStandalone = True
            , stateLiterateHaskell = True 
            }
        
        interpreter :: BBlock -> BBlock
        interpreter (Text (CodeBlock ("",[[x]],[]) e)) | x `elem` commandList 
            = OneLineExercise (toUpper x) (isUpper x) e
        interpreter a = a
        
        highlight :: BBlock -> BBlock
        highlight (Text (CodeBlock ("",["sourceCode","literate","haskell"],[]) code)) | mode == AgdaMode
            = (Text (RawBlock "html" $ highlightAgdaAsXHtml code))
        highlight a = a

------------------------------

collectTests :: ParseMode -> [BBlock] -> IO [BBlock]
collectTests mode l = zipWithM f l $ tail $ tails l where

    f (Text (CodeBlock ("",["sourceCode","literate","haskell"],[]) h)) l = do
        let
            isExercise = True -- not $ null $ concatMap fst exps

        (visible, hidden, funnames) <- processLines mode isExercise h
        let
            exps = [snd $ getCommand e | (OneLineExercise _ _ e) <- takeWhile p l]

            p (OneLineExercise x _ e) = x `elem` testCommandList && fst (getCommand e) == ""
            p _ = False

        return $ Exercise (lines h) visible hidden funnames exps

    f x _ = return x

processLines :: ParseMode -> Bool -> String -> IO ([String], [String], [Name])
processLines AgdaMode    = processAgdaLines
processLines HaskellMode = processHaskellLines

{- Agda support (unfinished) -}
processAgdaLines :: Bool -> String -> IO ([String], [String], [Name])
processAgdaLines isExercise l_ = do
    return ([], [], [])
{-
    let
        l = parts l_

    x <- fmap (zip l) $ mapM (Agda.parse Agda.moduleParser . ("module X where\n"++) . unlines) l
    let
        names = map toName $ concatMap (getFName . snd . snd) x

--        getFName [Agda.Module _ _ [Agda.TypedBindings _ (Agda.Arg _ _ [Agda.TBind _ a _])] declarations] 
--                  = map Agda.boundName a
        getFName [Agda.Module _ _ _ [Agda.TypeSig _ n _]]
                  = [n]
        getFName _ = []

--        isVisible [Agda.Module _ _ [Agda.TypedBindings _ (Agda.Arg _ _ [Agda.TBind _ a _])] declarations] 
--                    = True
        isVisible [Agda.Module _ _ _ [Agda.TypeSig _ n _]] = True
        isVisible _ = not isExercise

        (visible, hidden) = partition (isVisible . snd . snd) x

        toName n =  Ident $ show n

    return (concatMap fst visible, concatMap fst hidden, names)
-}

processHaskellLines :: Bool -> String -> IO ([String], [String], [Name])
processHaskellLines isExercise l_ = return (concatMap fst visible, concatMap fst hidden, names)
 where
    x = zip l $ map (HPar.parseDeclWithMode HPar.defaultParseMode . unlines) l

    l = parts l_

    names = concatMap (getFName . snd) x

    getFName (HPar.ParseOk x) = case x of
        HSyn.TypeSig _ a _                       -> map printName a
        HSyn.PatBind _ (HSyn.PVar a) _ _ _       -> [printName a]
        HSyn.FunBind (HSyn.Match _ a _ _ _ _ :_) -> [printName a]
        HSyn.TypeDecl _ a _ _                    -> [printName a]
        HSyn.DataDecl _ _ _ a _ x _              -> printName a: [printName n | HSyn.QualConDecl _ _ _ y<-x, n <- getN y]
        _                                        -> []
    getFName _ = []

    getN (HSyn.ConDecl n _) = [n]
    getN (HSyn.InfixConDecl _ n _) = [n]
    getN (HSyn.RecDecl n l) = n: concatMap fst l

    isVisible (HPar.ParseOk (HSyn.TypeSig _ _ _)) = True
    isVisible (HPar.ParseOk (HSyn.InfixDecl _ _ _ _)) = True
    isVisible _ = not isExercise

    (visible, hidden) = partition (isVisible . snd) x


parts :: String -> [[String]]
parts = groupBy (const id `on` isIndented) . lines  where
    isIndented s | all isSpace s = True
    isIndented (' ':_) = True
    isIndented _ = False

------------------------------

getCommand :: String -> (String, String)
getCommand (':':'?': (dropSpace -> Just x)) 
    = ("?", x)
getCommand (':': (span isAlpha -> (c@(_:_), dropSpace -> Just x)))
    = (c, x)
getCommand s
    = ("", s)

dropSpace :: String -> Maybe String
dropSpace (' ':y) = Just $ dropWhile (==' ') y
dropSpace "" = Just ""
dropSpace _ = Nothing

parseQuickCheck :: String -> ([String], String)
parseQuickCheck s = case splitOn ";;" s of
    l -> (init l, last l)

printName :: HSyn.Name -> Name
printName (HSyn.Ident x) = x
printName (HSyn.Symbol x) = x
