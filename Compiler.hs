{-# LANGUAGE TemplateHaskell #-}
import Parse
import Compile
import Syntax
import Data.Map as Map
import Data.Set as Set
import Control.Monad.State
import Control.Monad.Reader
import Autoserialize
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as B hiding (writeFile)
import System.Environment
import Data.List

a |> f = f a


class Enc a where enc :: a -> B.Builder


instance Enc Int where enc = B.int32LE . fromIntegral
instance Enc a => Enc [a] where
    enc = mconcat . fmap enc

autoser ''Instr
instance Enc Instr where
    enc = uncurry binTup . encTup


binTup :: Int -> Int -> B.Builder
binTup a b = mconcat [enc a, enc b]

runCode c = (is, symtab) where
    st = runReaderT c symbolIndices
    (is, symtab) = execState st (
        mempty,
        SymTab{ symTabIdx = mempty, symBytes = mempty })
    symbolIndices = symTabIdx symtab



compileFile inFile outFile = do
    input <- readFile inFile
    let Just (clauses, querry) = parse program input
    let grouped = groupBy (\(Clause a _ _) (Clause b _ _) -> a==b) clauses
    let preds = (\lst@((Clause x _ _):xs) ->
            Pred x ((\(Clause _ s g) -> (s,g)) <$> lst)) <$> grouped
    let c = code (analizePred <$> preds, querry)
    let (is, symtab) = runCode c
    print is
    print symtab
    let is' = B.toLazyByteString $ enc is
    B.writeFile outFile
        (B.int32LE (fromIntegral $ B.length is' + 8)
        <> B.int32LE 0
        <> B.lazyByteString is'
        <> B.byteString (symBytes symtab))
    return ()


main = do
    args <- getArgs
    name <- getProgName
    case args of
        [inPath, outPath] -> compileFile inPath outPath
        _ -> putStrLn $ concat [
            "usage:\t", name, " <input file> <output file>",
            "\n\t", name, " cbn <input file> <output file>"
            ]

