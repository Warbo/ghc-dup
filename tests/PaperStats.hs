{-# LANGUAGE Rank2Types, ExistentialQuantification, RecordWildCards #-}

-- compile me with 
-- > ghc -with-rtsopts=-T -O

import Data.Word
import Data.List
import Data.List.Split
import Data.Bits
import Data.Function
import System.Environment
import System.Mem
import System.IO
import GHC.Stats
import Control.Monad
import System.Process
import Data.Maybe
import GHC.HeapView hiding (Box, value)
import Text.Printf
import Data.Ord
import Dup

-- Specification

type S = Word32
data Tree = Node Word32 [Tree]
firstChild (Node _ (t:_)) = t
value n = popCount n
succs n = [n * 3, n * 5, n * 7, n * 9]

-- CTree stuff 
newtype CTree = CTree { unCTree :: forall a. (S -> [a] -> a) -> a }
toCTree :: Tree -> CTree
toCTree (Node s ts) = CTree $ \f -> f s $ map (\t -> unCTree (toCTree t) f) ts
fromCTree :: CTree -> Tree
fromCTree ct = unCTree ct Node

ctree :: S -> CTree
ctree s = CTree $ \f -> f s $ map (\s' -> unCTree (ctree s') f) (succs s)


crate :: Int -> CTree -> Int
crate d t = unCTree t crate' !! d
  where crate' n st =  value n : map maximum (transpose st)


csolve :: CTree -> [S]
csolve t = fst (unCTree t csolve')
  where
  csolve' :: S -> [([S],[Int])] -> ([S],[Int])
  csolve' n rc = 
    ( n : pickedChild
    , value n : map maximum (transpose (map snd rc)))
    where
    pickedChild = fst (maximumBy (comparing ((!! depth) . snd)) rc)

-- UTree stuff
data UTree' = UNode S [UTree]
type UTree = () -> UTree'
utree n = \_ -> UNode n (map utree (succs n))

usolve :: UTree -> [S]
usolve t = usolve' (t ())
  where
  usolve' (UNode n ts) = n : usolve pickedChild
    where
    ratedChilds = [ (t, urate depth t) | t <- ts ]
    pickedChild = fst (maximumBy (comparing snd) ratedChilds)


urate :: Int -> UTree -> Int
urate 0 t = case t () of (UNode n _)  -> popCount n
urate d t = case t () of (UNode _ ts) -> maximum (map (urate (d-1)) ts)


-- Regular tree stuff

t1 = tree 1
tree n = Node n (map tree (succs n))

depth = 4

solve :: Tree -> [S]
solve (Node n ts) = n : solve pickedChild
  where
  ratedChilds = [ (t, rate depth t) | t <- ts ]
  pickedChild = fst (maximumBy (comparing snd) ratedChilds)


rate 0 (Node n _) = popCount n
rate d (Node _ ts) = maximum (map (rate (d-1)) ts)

solveDup t = case dup t of Box t -> solve t

solveDeepDup t = case deepDup t of Box t -> solve t

solveRateDup (Node n ts) = n :
    solveRateDup (fst (maximumBy (comparing snd) [ (t, rateDup depth t) | t <- ts ]))

solveRateRecDup (Node n ts) = n :
    solveRateRecDup (fst (maximumBy (comparing snd) [ (t, rateRecDup depth t) | t <- ts ]))

rateDup d t = case dup t of Box t2 -> rate d t2
{-# NOINLINE rateDup #-}

rateRecDup 0 t = case dup t of Box (Node n _) -> popCount n
rateRecDup d t = case dup t of Box (Node _ ts) -> maximum (map (rate (d-1)) ts)
{-# NOINLINE rateRecDup #-}


dosomethingwith :: Tree -> IO S
dosomethingwith t = return $! solve t !! 1
{-# NOINLINE dosomethingwith #-}

runSize = 10000
--runSize = 10

data Run = forall t . Run {
    gTree :: S -> t,
    gSolve :: t -> IO S,
    gDosomethingwith :: t -> IO S,
    gFirstChild :: t -> t,
    gEvalAll :: t -> IO S}


regularSolver :: (Tree -> [S]) -> Run
regularSolver s = Run
    tree
    (\t -> return $! s t !! 10000)
    (\t -> return $! solve t !! 1)
    firstChild
    (\t -> return $! solve t !! 10000)

data RunDesc = Original 
	| SolveDup 
	| RateDup 
--	| RateRecDup 
	| SolveDeepDup 
	| Unit 
	| Church
    deriving (Show, Read, Eq)

runDescDesc Original = "original"
runDescDesc SolveDup = "\\textsf{solveDup}"
runDescDesc SolveDeepDup = "\\textsf{solveDeepDup}"
runDescDesc RateDup = "\\textsf{rateDup}"
runDescDesc RateRecDup = "\\textsf{rateRecDup}"
runDescDesc Unit = "unit lifting"
runDescDesc Church = "church encoding"


runs :: [(RunDesc, Run)]
runs = [
    (Original, regularSolver solve),
    (SolveDup, regularSolver solveDup),
    (RateDup, regularSolver solveRateDup),
    (RateRecDup, regularSolver solveRateRecDup),
    (SolveDeepDup, regularSolver solveDeepDup),
    (Unit, Run
        utree
        (\t -> return $! usolve t !! 10000)
        (\t -> return $! usolve t !! 1)
        id
        (\t -> return $! usolve t !! 10000)
    ),
    (Church, Run
        ctree
        (\t -> return $! csolve t !! 10000)
        (\t -> return $! csolve t !! 1)
        id
        (\t -> return $! csolve t !! 10000)
    )
    ]

data Variant = Unshared | Shared | SharedThunk | SharedEvaled | SharedFull deriving (Read, Show, Enum, Bounded)

vardesc Unshared = "no sharing"
vardesc Shared = "shared tree"
vardesc SharedThunk = "add. thunk"
vardesc SharedEvaled = "partly eval'ed"
vardesc SharedFull = "fully eval'ed"

mainStats = do
    printf "\\makeatletter\n"
    printf "\\begin{tabular}{l"
    forM_ [minBound..maxBound::Variant] $ \variant -> do
        printf "rr"
    printf "}\n"
    printf " \\\\\n"
    forM_ [minBound..maxBound::Variant] $ \variant -> do
        printf "& \\multicolumn{2}{c}{%s}" (vardesc variant)
    printf " \\\\\n"
    forM_ [minBound..maxBound::Variant] $ \variant -> do
        printf "& MB & sec."
    printf " \\\\ \\midrule \n"
    hSetBuffering stdout NoBuffering
    forM_ (map fst runs) $ \run -> do
        printf "%s" (runDescDesc run)
        forM_ [minBound..maxBound::Variant] $ \variant -> do
            out <- readProcess "./PaperStats" [show run, show variant] ""
            let (_, _, alloc, time) = read out :: (String, Variant, Integer, Double)
            -- print (run, variant, alloc, time)
            printf "& {\\def\\@currentlabel{%s}\\label{stats:%s:%s:mem}%s}" (showLargeNum alloc) (show run) (show variant) (showLargeNum alloc)
            printf " & {\\def\\@currentlabel{%.2f}\\label{stats:%s:%s:time}%.2f}" time (show run) (show variant) time
            return ()
        printf " \\\\\n"
    printf "\\end{tabular}\n"
    printf "\\makeatother\n"

showLargeNum = intercalate "\\," . map reverse . reverse . splitEvery 3 . reverse . show 


mainRun :: RunDesc -> Variant -> S -> IO ()
mainRun n variant k = do
    case fromJust $ lookup n runs of
        Run{..} -> do
            case variant of
                Unshared -> do
                    let t = gTree k
                    gSolve t
                Shared -> do
                    let t = gTree k
                    gSolve t
                    gDosomethingwith t
                SharedThunk -> do
                    let t = gTree k
                    let t' = gFirstChild t
                    gSolve t'
                    gDosomethingwith t
                SharedEvaled -> do
                    let t = gTree k
                    gFirstChild t `seq` return ()
                    gSolve t
                    gDosomethingwith t
                SharedFull -> do
                    let t = gTree k
                    gEvalAll t
                    performGC
                    gSolve t
                    gDosomethingwith t
    performGC
    stats <- getGCStats
    print (show n, variant, peakMegabytesAllocated stats, cpuSeconds stats)

main = do
    args <- getArgs
    case args of 
        [] -> mainStats
        [n,s] -> mainRun (read n) (read s) (fromIntegral (length args))
{-
main = do
    [n] <- getArgs 
    let k = read n
    print k
    performGC
    let t = tree k

    --t `seq` return ()
    --print $ rate 1 t

    print $ solve t !! runSize
    print $ solve t !! runSize
    hFlush stdout
    print $ solve t !! 1

-}
