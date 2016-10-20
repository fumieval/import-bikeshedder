{-# LANGUAGE LambdaCase, FlexibleContexts, ViewPatterns, DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Char
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Ord
import System.Environment (getArgs)
import System.IO
import System.Process
import Text.Parser.Token.Style
import Text.Trifecta
import System.Random
import Data.Hashable
import GHC.Generics (Generic)

data Import = Import
    Bool -- qualified
    (Maybe String) -- "package name"
    [String] -- Module.Name
    (Maybe String) -- as
    Bool -- hiding
    (Maybe [String]) -- (impspec)
    | Str String
    deriving (Show, Eq, Ord, Generic)

instance Hashable Import

moduleName :: Traversal' Import [String]
moduleName f (Import qual pkg m q hide ss) = f m
    <&> \m' -> Import qual pkg m' q hide ss
moduleName _ other = pure other

imports :: Lens' String [Import]
imports f src = case parseString (parser <* eof) mempty src of
      Success result
          | (header, xs) <- break (has moduleName) result
          , (ydob, ys) <- break (has moduleName) (reverse xs)
          -> f (reverse ys) <&> \ys' -> unlines
              $ map printImport
              $ header ++ ys' ++ reverse ydob
      Failure e -> error (show e)
  where
    parser = many $ try parseImport <|> Str <$> manyTill anyChar (try newline)

parseSymbols :: Parser [String]
parseSymbols = do
    symbol "("
    ss <- ((opr <|> ident haskellIdents) <> option "" inner) `sepBy` (symbol ",")
    spaces
    string ")"
    return ss
  where
    opr = token $ string "(" <> ident haskellOps <> string ")"
    inner = parens $ do
        xs <- (symbol ".." <|> opr <|> ident haskellIdents) `sepBy` (symbol ",")
        return $ "(" ++ intercalate ", " xs ++ ")"

parseImport :: Parser Import
parseImport = runUnlined $ do
    symbol "import"
    qual <- isJust <$> optional (symbol "qualified")
    pkg <- optional stringLiteral
    name <- ident haskellIdents `sepBy` char '.'
    a <- optional $ symbol "as" *> ident haskellIdents
    hide <- isJust <$> optional (symbol "hiding")
    ss <- optional (Unlined parseSymbols)
    newline
    return $ Import qual pkg name a hide ss

printImport :: Import -> String
printImport (Import qual pkg name a hide ss) = unwords $ concat
    [ ["import"]
    , ["qualified" | qual]
    , ["\"" ++ p ++ "\"" | p <- pkg ^.. folded]
    , [intercalate "." name]
    , ["as " ++ q | q <- a ^.. folded]
    , ["hiding" | hide]
    , ["(" ++ intercalate ", " xs ++ ")" | xs <- ss ^.. folded]
    ]
printImport (Str str) = str

sortImports :: Ord group => (Import -> group) -> [Import] -> [Import]
sortImports k xs = intercalate [Str ""]
    $ map (sortBy (comparing (previews moduleName (map (map toLower))) <> compare) . ($[]))
    $ Map.elems $ Map.fromListWith (.) [(k i, (i:)) | i <- xs]

parseGroup :: Parser (Int, [[String]])
parseGroup = runUnlined $ do
    i <- integer
    newline
    xs <- some $ do
        some space
        m <- ident haskellIdents `sepBy` char '.'
        newline
        return m
    return (fromIntegral i, xs)

loadGroupFun :: FilePath -> IO (Import -> Int)
loadGroupFun path = do
    Just gs <- parseFromFile (many parseGroup) path
    return $ \imp -> case imp ^? moduleName of
        Nothing -> 0
        Just m -> case maximumByOf traverse (comparing fst)
            [(length ps, p) | (p, ps) <- gs, any (`isPrefixOf`m) ps] of
                Just (_, p) -> p
                _ -> 0

dispatch :: FilePath -> [String] -> [Import] -> IO [Import]
dispatch path ("sortby" : spec : r) xs = do
    grp <- loadGroupFun spec
    dispatch path r $ sortImports grp $ filter (/=Str "") xs
dispatch path ("sort" : r) xs = dispatch path r $ sortImports (const ()) $ filter (/=Str "") xs
dispatch path ("shuffle" : r) xs = do
    i <- randomIO
    dispatch path r $ sortOn (hashWithSalt i) xs
dispatch path ("debug" : r) xs = do
    hPutStrLn stderr path
    mapM_ (hPutStrLn stderr . show) xs
    dispatch path r xs
dispatch _ [] xs = return xs
dispatch _ s _ = fail $ "Unknown function: " ++ show s

main :: IO ()
main = do
    (funs, ps) <- break (=="--") <$> getArgs
    case ps of
        ("--":paths) -> forM_ paths $ \path -> do
            src <- readFile path >>= imports (dispatch path funs)
            (_, result, _) <- readProcessWithExitCode "/usr/bin/diff"
                ["-u", path, "-"]
                src
            putStr $ result & iso lines unlines %~
                ( ix 0 . iso words unwords . ix 1 %~ ("a/"++) )
                . (ix 1 . iso words unwords . ix 1 .~ "b/" ++ path)
        _ -> fail "import-bikeshedder [sort|sortby (rules)|shuffle|debug] -- [paths]"
