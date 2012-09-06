{-# LANGUAGE Rank2Types, ExistentialQuantification, RecordWildCards #-}

-- compile me with 
-- > ghc -with-rtsopts=-T -O

import Data.Word
import Data.List
--import Data.List.Split
import Data.Bits
import Data.Function
import System.Environment
import System.Mem
import System.IO
import GHC.Stats
import Control.Monad
import System.Process
import Data.Maybe
--import GHC.HeapView hiding (Box, value)
import Text.Printf
import Data.Ord
import GHC.Dup

-- Specification

type S = Word32
data Tree = Node Word32 [Tree]
firstChild (Node _ (t:_)) = t
value n = popCount n

countDown :: Word32 -> Bool
countDown 0 = True
countDown n = countDown (n-1)
{-# NOINLINE countDown #-}

succs n = [n * 3, n * 5, n * 7, n * 9]
succsSlow n = if countDown (min (2^23) n) then [n * 3, n * 5, n * 7, n * 9] else []

-- CTree stuff 
newtype CTree = CTree { unCTree :: forall a. (S -> [a] -> a) -> a }
toCTree :: Tree -> CTree
toCTree (Node s ts) = CTree $ \f -> f s $ map (\t -> unCTree (toCTree t) f) ts
fromCTree :: CTree -> Tree
fromCTree ct = unCTree ct Node

ctree :: S -> CTree
ctree s = CTree $ \f -> f s $ map (\s' -> unCTree (ctree s') f) (succs s)

ctreeSlow :: S -> CTree
ctreeSlow s = CTree $ \f -> f s $ map (\s' -> unCTree (ctreeSlow s') f) (succsSlow s)


crate :: Int -> CTree -> Int
crate d t = unCTree t crate' d
  where
  crate' :: S -> [Int -> Int] -> (Int -> Int)
  crate' n st 0 = value n
  crate' n st d = maximum (map ($ d-1) st)


csolve :: CTree -> [S]
csolve t = fst (unCTree t csolve')
  where
  csolve' :: S -> [([S],Int -> Int)] -> ([S],Int -> Int)
  csolve' n rc = 
    ( n : pickedChild
    , \d -> if d == 0
            then value n
            else maximum (map (($ d-1) . snd) rc))
    where
    pickedChild = fst (maximumBy (comparing (($ depth) . snd)) rc)


-- UTree stuff
data UTree' = UNode S [UTree]
type UTree = () -> UTree'
utree n = \_ -> UNode n (map utree (succs n))
utreeSlow n = \_ -> UNode n (map utreeSlow (succsSlow n))

usolve :: UTree -> [S]
usolve t = usolve' (t ())
  where
  usolve' (UNode n ts) = n : usolve pickedChild
    where
    ratedChilds = [ (t, urate depth t) | t <- ts ]
    pickedChild = fst (maximumBy (comparing snd) ratedChilds)

urate :: Int -> UTree -> Int
urate 0 t = case t () of (UNode n _)  -> value n
urate d t = case t () of (UNode _ ts) -> maximum (map (urate (d-1)) ts)


-- Regular tree stuff

t1 = tree 1
tree n = Node n (map tree (succs n))
treeSlow n = Node n (map treeSlow (succsSlow n))

depth = 4

solve :: Tree -> [S]
solve (Node n ts) = n : solve pickedChild
  where
  ratedChilds = [ (t, rate depth t) | t <- ts ]
  pickedChild = fst (maximumBy (comparing snd) ratedChilds)

rate 0 (Node n _) = value n
rate d (Node _ ts) = maximum (map (rate (d-1)) ts)

solveDup t = case dup t of Box t -> solve t

solveDeepDup t = case deepDup t of Box t -> solve t

solveRateDup (Node n ts) = n :
    solveRateDup (fst (maximumBy (comparing snd) [ (t, rateDup depth t) | t <- ts ]))

solveRateRecDup (Node n ts) = n :
    solveRateRecDup (fst (maximumBy (comparing snd) [ (t, rateRecDup depth t) | t <- ts ]))

solveDeepDupRateDup t = case deepDup t of Box t -> go t
    where go (Node n ts) = n :
            go (fst (maximumBy (comparing snd) [ (t, rateDup depth t) | t <- ts ]))

rateDup d t = case dup t of Box t2 -> rate d t2
{-# NOINLINE rateDup #-}

rateRecDup 0 t = case dup t of Box (Node n _) -> value n
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

regularSolverSlow :: (Tree -> [S]) -> Run
regularSolverSlow s = Run
    treeSlow
    (\t -> return $! s t !! 10000)
    (\t -> return $! solve t !! 1)
    firstChild
    (\t -> return $! solve t !! 10000)

data RunDesc = Original 
	| SolveDup 
	| RateDup 
	| RateRecDup 
	| SolveDeepDup 
	| SolveDeepDupRateDup
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


runs :: [((Bool,RunDesc), Run)]
runs = [
    ((False,Original), regularSolver solve),
    ((False,SolveDup), regularSolver solveDup),
    ((False,RateDup), regularSolver solveRateDup),
--    ((False,RateRecDup), regularSolver solveRateRecDup),
    ((False,SolveDeepDup), regularSolver solveDeepDup),
--    ((False,SolveDeepDupRateDup), regularSolver solveDeepDupRateDup),
    ((False,Unit), Run
        utree
        (\t -> return $! usolve t !! 10000)
        (\t -> return $! usolve t !! 1)
        id
        (\t -> return $! usolve t !! 10000)
    ),
    ((False,Church), Run
        ctree
        (\t -> return $! csolve t !! 10000)
        (\t -> return $! csolve t !! 1)
        id
        (\t -> return $! csolve t !! 10000)
    ),
    ((True,Original), regularSolverSlow solve),
    ((True,SolveDup), regularSolverSlow solveDup),
    ((True,RateDup), regularSolverSlow solveRateDup),
--    ((True,RateRecDup), regularSolver solveRateRecDup),
    ((True,SolveDeepDup), regularSolverSlow solveDeepDup),
    ((True,Unit), Run
        utreeSlow
        (\t -> return $! usolve t !! 10000)
        (\t -> return $! usolve t !! 1)
        id
        (\t -> return $! usolve t !! 10000)
    ),
    ((True,Church), Run
        ctreeSlow
        (\t -> return $! csolve t !! 10000)
        (\t -> return $! csolve t !! 1)
        id
        (\t -> return $! csolve t !! 10000)
    )
    ]

data Variant = Unshared | Shared | SharedThunk | SharedEvaled | SharedFull | RunTwice deriving (Eq, Read, Show, Enum, Bounded)

vardesc Unshared = "no sharing"
vardesc Shared = "shared tree"
vardesc SharedThunk = "add. thunk"
vardesc SharedEvaled = "partly eval'ed"
vardesc SharedFull = "fully eval'ed"
vardesc RunTwice = "run twice"

skipped = [(SharedEvaled, Unit), (SharedEvaled, Church),
           (SharedFull, Unit), (SharedFull, Church)]

mainStats slow = do
    let slowT = if slow then "slow:" else ""
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
    forM_ (map fst runs) $ \run -> when (fst run == slow) $ do
        printf "%s%%\n" (runDescDesc (snd run))
        forM_ [minBound..maxBound::Variant] $ \variant ->
            if (variant, snd run) `elem` skipped
            then putStr "&\n&\n"
            else do
            out <- readProcess "./PaperStats" [show run, show variant] ""
            let (_, _, alloc, time) = read out :: (String, Variant, Integer, Double)
            -- print (run, variant, alloc, time)
            printf "&\n {\\def\\@currentlabel{%s}\\label{stats:%s%s:%s:mem}%s}" (showLargeNum alloc) slowT (show (snd run)) (show variant) (showLargeNum alloc)
            printf " &\n {\\def\\@currentlabel{%.2f}\\label{stats:%s%s:%s:time}%.2f}" time slowT (show (snd run)) (show variant) time
            return ()
        printf " \\\\\n"
    printf "\\end{tabular}\n"
    printf "\\makeatother\n"

showLargeNum = intercalate "\\," . map reverse . reverse . splitEvery 3 . reverse . show 

splitEvery _ [] = []
splitEvery n l = take n l : splitEvery n (drop n l)


mainRun :: (Bool, RunDesc) -> Variant -> S -> IO ()
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
                    performGC
                    gDosomethingwith t
                SharedThunk -> do
                    let t = gTree k
                    let t' = gFirstChild t
                    gSolve t'
                    performGC
                    gDosomethingwith t
                SharedEvaled -> do
                    let t = gTree k
                    gFirstChild t `seq` return ()
                    performGC
                    gSolve t
                    performGC
                    gDosomethingwith t
                SharedFull -> do
                    let t = gTree k
                    gEvalAll t
                    performGC
                    gSolve t
                    performGC
                    gDosomethingwith t
                RunTwice -> do
                    let t = gTree k
                    gSolve t
                    performGC
                    gSolve t
                    performGC
                    gDosomethingwith t
    performGC
    stats <- getGCStats
    print (show n, variant, peakMegabytesAllocated stats, cpuSeconds stats)

main = do
    args <- getArgs
    case args of 
        [] -> mainStats False
        ["slow"] -> mainStats True
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
