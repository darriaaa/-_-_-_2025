{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import GHC.Conc (getNumCapabilities)
import System.Environment (getArgs)
import System.Exit (die)

import Graph
import ParallelBellmanFord

data Options = Options
  { optThreads     :: !Int
  , optSource      :: !Int
  , optTarget      :: !Int
  , optInput       :: !(Maybe FilePath)
  , optVertices    :: !Int
  , optEdges       :: !Int
  , optMaxWeight   :: !Int
  , optUndirected  :: !Bool
  , optBench       :: !Bool
  , optMaxThreads  :: !Int
  } deriving (Show)

defaultOptions :: Options
defaultOptions = Options
  { optThreads    = 4
  , optSource     = 0
  , optTarget     = 1
  , optInput      = Just "data/sample.graph"
  , optVertices   = 2000
  , optEdges      = 20000
  , optMaxWeight  = 20
  , optUndirected = False
  , optBench      = False
  , optMaxThreads = 8
  }

usage :: String
usage = unlines
  [ "Parallel shortest path (weighted graph) — Lab 2"
  , ""
  , "Usage:"
  , "  cabal run parallel-shortest-path -- [OPTIONS] +RTS -N -RTS"
  , ""
  , "Options:"
  , "  --input FILE            Read graph from file (default: data/sample.graph)"
  , "  --generate              Generate random graph instead of reading --input"
  , "  --vertices N            Number of vertices for generated graph (default: 2000)"
  , "  --edges M               Number of edges for generated graph (default: 20000)"
  , "  --max-weight W          Max edge weight for generated graph (default: 20)"
  , "  --undirected            Generate undirected graph (adds reverse edges)"
  , "  --source S              Source vertex id (0..V-1)"
  , "  --target T              Target vertex id (0..V-1)"
  , "  --threads N             Worker threads (tasks) for parallel relaxation (default: 4)"
  , "  --bench                 Run benchmark for 1..--max-threads workers and print timings"
  , "  --max-threads N         Max workers for --bench (default: 8)"
  , "  --help                  Print this help"
  , ""
  , "Graph file format:"
  , "  First non-comment line: V E"
  , "  Then E lines: u v w"
  , "  (0-based vertices, directed edges)"
  ]

main :: IO ()
main = do
  args <- getArgs
  let optsE = parseArgs defaultOptions args
  opts <- either die pure optsE

  caps <- getNumCapabilities
  putStrLn $ "GHC capabilities (RTS): " <> show caps
  putStrLn $ "Options: " <> show opts

  graph <- case optInput opts of
    Just fp | not (wantsGenerate args) -> readGraphFile fp
    _ -> do
      putStrLn "Generating random graph..."
      generateRandomGraph
        (optVertices opts)
        (optEdges opts)
        (optMaxWeight opts)
        (optUndirected opts)

  let vCount = graphVertices graph
      eCount = V.length (graphEdges graph)
  putStrLn $ "Graph loaded: V=" <> show vCount <> ", E=" <> show eCount

  if optBench opts
    then runBench opts graph
    else runOnce opts graph

wantsGenerate :: [String] -> Bool
wantsGenerate = any (== "--generate")

runOnce :: Options -> Graph -> IO ()
runOnce opts graph = do
  validateST opts graph
  t0 <- getCurrentTime
  result <- shortestPathParallel
    (optThreads opts)
    graph
    (optSource opts)
    (optTarget opts)
  t1 <- getCurrentTime
  putStrLn $ "Elapsed: " <> show (diffUTCTime t1 t0)
  case result of
    Nothing -> putStrLn "No path found."
    Just (dist, path) -> do
      putStrLn $ "Distance: " <> show dist
      putStrLn $ "Path: " <> intercalate " -> " (map show path)

runBench :: Options -> Graph -> IO ()
runBench opts graph = do
  validateST opts graph
  let maxT = max 1 (optMaxThreads opts)
      threadList = [1..maxT]
  putStrLn ""
  putStrLn "threads\ttime_sec\tdistance"
  baseTime <- go 1
  mapM_ (goWith baseTime) (drop 1 threadList)
  where
    go :: Int -> IO Double
    go n = do
      t0 <- getCurrentTime
      res <- shortestPathParallel n graph (optSource opts) (optTarget opts)
      t1 <- getCurrentTime
      let dt = realToFrac (diffUTCTime t1 t0) :: Double
      putStrLn $ show n <> "\t" <> show dt <> "\t" <> show (maybe (-1) fst res)
      pure dt

    goWith :: Double -> Int -> IO ()
    goWith base n = do
      t0 <- getCurrentTime
      res <- shortestPathParallel n graph (optSource opts) (optTarget opts)
      t1 <- getCurrentTime
      let dt = realToFrac (diffUTCTime t1 t0) :: Double
          speedup = if dt > 0 then base / dt else 0
      putStrLn $ show n <> "\t" <> show dt <> "\t" <> show (maybe (-1) fst res) <> "   (speedup x" <> showFF speedup <> ")"

showFF :: Double -> String
showFF x = let s = show (fromIntegral (round (x * 100) :: Int) / 100 :: Double) in s

validateST :: Options -> Graph -> IO ()
validateST opts graph = do
  let v = graphVertices graph
      s = optSource opts
      t = optTarget opts
  if s < 0 || s >= v then die "Invalid --source (out of range)." else pure ()
  if t < 0 || t >= v then die "Invalid --target (out of range)." else pure ()
  if optThreads opts < 1 then die "Invalid --threads (must be >= 1)." else pure ()

parseArgs :: Options -> [String] -> Either String Options
parseArgs o [] = Right o
parseArgs _ ("--help":_) = Left usage
parseArgs o ("--bench":xs) = parseArgs (o { optBench = True }) xs
parseArgs o ("--undirected":xs) = parseArgs (o { optUndirected = True }) xs
parseArgs o ("--generate":xs) = parseArgs (o { optInput = Nothing }) xs
parseArgs o ("--input":fp:xs) = parseArgs (o { optInput = Just fp }) xs
parseArgs o ("--vertices":n:xs) = parseArgs (o { optVertices = readInt "--vertices" n }) xs
parseArgs o ("--edges":n:xs) = parseArgs (o { optEdges = readInt "--edges" n }) xs
parseArgs o ("--max-weight":n:xs) = parseArgs (o { optMaxWeight = readInt "--max-weight" n }) xs
parseArgs o ("--source":n:xs) = parseArgs (o { optSource = readInt "--source" n }) xs
parseArgs o ("--target":n:xs) = parseArgs (o { optTarget = readInt "--target" n }) xs
parseArgs o ("--threads":n:xs) = parseArgs (o { optThreads = readInt "--threads" n }) xs
parseArgs o ("--max-threads":n:xs) = parseArgs (o { optMaxThreads = readInt "--max-threads" n }) xs
parseArgs _ (x:_) = Left ("Unknown option: " <> x <> "\n\n" <> usage)

readInt :: String -> String -> Int
readInt flag s =
  case reads s of
    [(n,"")] -> n
    _ -> error ("Invalid integer for " <> flag <> ": " <> s)
