{-# language DataKinds #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language GeneralizedNewtypeDeriving #-}

module Main where

import Control.Lens
import Language.Wasm
import Language.Wasm.Structure
import Language.Wasm.Binary
import Options.Applicative
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (foldl', sortOn, partition, sort)
import Text.Pretty.Simple
import Numeric.Natural (Natural)

data InputOpts = InputOpts
  { wasmFileName :: FilePath
  }

inputOpts = InputOpts
  <$> strOption
    (long "input"
    <> metavar "WASM_FILE"
    <> help "Input file in wasm binary format")

main :: IO ()
main = do
  opts <- execParser (info (inputOpts <**> helper)
    (fullDesc))

  wasmFile <- BS.readFile (wasmFileName opts)
  let stats = getStats <$> wmod
      wmod = decodeModuleLazy wasmFile
  let
    (Right dist) =
       (funcSizeDistribution) <$> stats

    fsGrps = take 100 . reverse
       . (sortOn (fst . snd))
       . Map.toList $ dist

    bins = over (each . _2 . _2)
      (findBins doExprDiff) fsGrps

    doExprDiff (Function _ _ b1) (Function _ _ b2) = case exprDiff b1 b2 of
      NoDiff -> True
      MinorDiff _ -> True
      VeryDifferent _ -> False

    binLengths = over (each . _2 . _2) (take 5 . reverse . sort . (map length)) bins

  mapM_ printResult binLengths
  -- pPrintNoColor $ f
  -- pPrintNoColor $ take 4 $ drop 22 fs

printResult ((s, _, _), (c,ls)) = do
  putStrLn $ show s ++ " x " ++ show c ++ " : " ++ show ls

findBins :: (a -> a -> Bool) -> [a] -> [[a]]
findBins _ [] = []
findBins g (f:fs) = (f:a) :
  if (length b > 100) then (findBins g b) else []
  where (a,b) = partition (g f) fs

newtype Count = Count Int
  deriving (Show, Ord, Eq, Num)

newtype Size = Size Int
  deriving (Show, Ord, Eq, Num)

data Stats = Stats
  { funcCount :: Count
  , funcSizeDistribution :: Map (Size, TypeIndex, LocalsType) (Count, [Function])
  }
  deriving (Show)

getStats :: Module -> Stats
getStats wmod = Stats c dist
  where
    c = (Count . length . functions) wmod
    dist = foldl' getDist Map.empty (functions wmod)

    getDist m f@(Function t l b) = Map.alter g (getExpSize b, t, l)  m
      where g Nothing = Just (Count 1, [f])
            g (Just (Count c, fs)) = Just (Count (c + 1), f:fs)

getExpSize :: Expression -> Size
getExpSize [] = Size 0
getExpSize (i:is) = e + (getExpSize is)
  where e = case i of
              (Block _ b) -> getExpSize b
              (Loop _ b) -> getExpSize b
              (If _ t f) -> getExpSize t + getExpSize f
              _ -> Size 1


data Diff
  = VeryDifferent [(Instruction Natural, Instruction Natural)]
  | MinorDiff [(Instruction Natural, Instruction Natural)]
  | NoDiff
  deriving (Show)

-- Input Expression of same size
exprDiff :: Expression -> Expression -> Diff
exprDiff f1 f2 = loop f1 f2
  where
    loop [] [] = NoDiff
    loop (i1:is1) (i2:is2) = case (g i1 i2) of
      NoDiff -> loop is1 is2
      MinorDiff ds -> case (loop is1 is2) of
        NoDiff -> MinorDiff ds
        MinorDiff ds2 -> MinorDiff (ds ++ ds2)
        VeryDifferent ds2 -> VeryDifferent ds2
      VeryDifferent ds2 -> VeryDifferent ds2
    loop _ _ = error "loop got different lengths?"

    g (i1@(Block w1 b1)) (i2@(Block w2 b2)) =
      if (w1 == w2)
        then exprDiff b1 b2
        else VeryDifferent []

    g (i1@(I32Const w1)) (i2@(I32Const w2)) =
      if (w1 == w2) then NoDiff else MinorDiff [(i1,i2)]

    -- g (i1@(I32Load w1)) (i2@(I32Load w2)) =
    --   if (w1 == w2) then NoDiff else MinorDiff [(i1,i2)]

    g i1 i2 =
      if (i1 == i2) then NoDiff else VeryDifferent [(i1,i2)]
