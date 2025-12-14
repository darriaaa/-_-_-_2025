{-# LANGUAGE BangPatterns #-}

module Graph
  ( Edge(..)
  , Graph(..)
  , readGraphFile
  , generateRandomGraph
  ) where

import Control.Monad (when)
import Data.Int (Int64)
import Data.List (isPrefixOf)
import System.Random (StdGen, mkStdGen, randomR)
import qualified Data.Vector as V

data Edge = Edge
  { eFrom   :: !Int
  , eTo     :: !Int
  , eWeight :: !Int64
  } deriving (Show, Eq)

data Graph = Graph
  { graphVertices :: !Int
  , graphEdges    :: !(V.Vector Edge)
  } deriving (Show)


readGraphFile :: FilePath -> IO Graph
readGraphFile fp = do
  content <- readFile fp
  let ls = filter (not . null) $ map strip $ filter (not . isComment) (lines content)
  case ls of
    [] -> fail "Graph file is empty."
    (hdr:rest) -> do
      let ws = words hdr
      when (length ws < 2) $ fail "Invalid header. Expected: V E"
      let v = read (ws !! 0) :: Int
          e = read (ws !! 1) :: Int
      edges <- parseEdges e rest
      pure $ Graph v (V.fromList edges)

parseEdges :: Int -> [String] -> IO [Edge]
parseEdges expected xs = do
  let edgeLines = take expected xs
  when (length edgeLines < expected) $
    fail ("Not enough edge lines. Expected " <> show expected)
  pure (map parseEdge edgeLines)

parseEdge :: String -> Edge
parseEdge line =
  case words line of
    [a,b,c] -> Edge (read a) (read b) (read c)
    _ -> error ("Invalid edge line: " <> line)

isComment :: String -> Bool
isComment s = "#" `isPrefixOf` strip s

strip :: String -> String
strip = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

generateRandomGraph :: Int -> Int -> Int -> Bool -> IO Graph
generateRandomGraph v e maxW undirected = do
  when (v < 2) $ fail "vertices must be >= 2"
  when (e < v - 1) $ fail "edges must be >= V-1 (to keep chain edges)"


  let g0 = mkStdGen 42

  let (chainEdges, g1) = buildChain g0
      remaining = e - (v - 1)
      (rndEdges, _) = buildRandomEdges remaining g1

  let baseEdges = chainEdges ++ rndEdges
      allEdges  = if undirected
        then baseEdges ++ map rev baseEdges
        else baseEdges

  pure $ Graph v (V.fromList allEdges)
  where
    buildChain :: StdGen -> ([Edge], StdGen)
    buildChain g = go 0 g []
      where
        go i gen acc
          | i >= v - 1 = (reverse acc, gen)
          | otherwise =
              let (w, gen') = randW gen
                  !edge = Edge i (i+1) w
              in go (i+1) gen' (edge:acc)

    buildRandomEdges :: Int -> StdGen -> ([Edge], StdGen)
    buildRandomEdges k g = go k g []
      where
        go 0 gen acc = (acc, gen)
        go n gen acc =
          let (u, gen1) = randomR (0, v-1) gen
              (t, gen2) = randomR (0, v-1) gen1
              (w, gen3) = randW gen2
              to' = if u == t then (t + 1) `mod` v else t
              !edge = Edge u to' w
          in go (n-1) gen3 (edge:acc)

    randW :: StdGen -> (Int64, StdGen)
    randW gen =
      let (w, gen') = randomR (1, max 1 maxW) gen
      in (fromIntegral (w :: Int), gen')

    rev (Edge u t w) = Edge t u w
