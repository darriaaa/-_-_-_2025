{-# LANGUAGE BangPatterns #-}

module ParallelBellmanFord
  ( shortestPathParallel
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM_, when)
import Data.Int (Int64)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

import Graph (Graph(..), Edge(..))


shortestPathParallel
  :: Int          -- ^ worker tasks (n threads)
  -> Graph
  -> Int          -- ^ source
  -> Int          -- ^ target
  -> IO (Maybe (Int64, [Int]))   -- ^ (distance, path)
shortestPathParallel workers (Graph n edges) src tgt = do
  let inf = maxBound `div` 4 :: Int64
      dist0 = U.generate n (\i -> if i == src then 0 else inf)
      parent0 = U.replicate n (-1 :: Int)

      chunks = splitEdges workers edges

  (distF, parentF) <- loop inf chunks (n - 1) dist0 parent0
  let dT = distF U.! tgt
  if dT >= inf
    then pure Nothing
    else pure (Just (dT, reconstructPath src tgt parentF))


loop
  :: Int64
  -> [V.Vector Edge]
  -> Int
  -> U.Vector Int64
  -> U.Vector Int
  -> IO (U.Vector Int64, U.Vector Int)
loop _ _ 0 dist parent = pure (dist, parent)
loop inf chunks k dist parent = do
  proposals <- mapConcurrently (relaxChunk inf dist) chunks
  (dist', parent', changed) <- mergeProposals inf dist parent proposals
  if not changed
    then pure (dist', parent')
    else loop inf chunks (k - 1) dist' parent'


relaxChunk
  :: Int64
  -> U.Vector Int64
  -> V.Vector Edge
  -> IO (U.Vector Int64, U.Vector Int)
relaxChunk inf distPrev chunk = do
  bestD <- UM.replicate (U.length distPrev) inf
  bestP <- UM.replicate (U.length distPrev) (-1 :: Int)

  V.forM_ chunk $ \(Edge u v w) -> do
    let du = distPrev U.! u
    when (du < inf) $ do
      let cand = du + w
      cur <- UM.read bestD v
      when (cand < cur) $ do
        UM.write bestD v cand
        UM.write bestP v u

  d <- U.freeze bestD
  p <- U.freeze bestP
  pure (d, p)


mergeProposals
  :: Int64
  -> U.Vector Int64
  -> U.Vector Int
  -> [(U.Vector Int64, U.Vector Int)]
  -> IO (U.Vector Int64, U.Vector Int, Bool)
mergeProposals inf distPrev parentPrev props = do
  distNext <- U.thaw distPrev
  parentNext <- U.thaw parentPrev
  changedRef <- newIORef False
  let !n = U.length distPrev

  forM_ [0 .. n - 1] $ \v -> do
    let !baseD = distPrev U.! v
        !baseP = parentPrev U.! v
    (!bestD, !bestP) <- foldMProps baseD baseP v props
    when (bestD < baseD) $ do
      UM.write distNext v bestD
      UM.write parentNext v bestP
      writeIORef changedRef True


  d <- U.freeze distNext
  p <- U.freeze parentNext
  ch <- readIORef changedRef
  pure (d, p, ch)
  where
    foldMProps :: Int64 -> Int -> Int -> [(U.Vector Int64, U.Vector Int)] -> IO (Int64, Int)
    foldMProps !bestD !bestP _ [] = pure (bestD, bestP)
    foldMProps !bestD !bestP v ((dvec, pvec):xs) =
      let candD = dvec U.! v
      in if candD < bestD
           then foldMProps candD (pvec U.! v) v xs
           else foldMProps bestD bestP v xs

splitEdges :: Int -> V.Vector Edge -> [V.Vector Edge]
splitEdges workers es
  | workers <= 1 = [es]
  | otherwise =
      let n = V.length es
          chunkSize = (n + workers - 1) `div` workers
      in [ V.slice (i * chunkSize) (min chunkSize (n - i * chunkSize)) es
         | i <- [0 .. workers - 1]
         , i * chunkSize < n
         ]

reconstructPath :: Int -> Int -> U.Vector Int -> [Int]
reconstructPath src tgt parent = reverse (go tgt [])
  where
    go !v acc
      | v == src = src : acc
      | v < 0 = acc
      | otherwise =
          let p = parent U.! v
          in if p == (-1) then (v:acc) else go p (v:acc)
