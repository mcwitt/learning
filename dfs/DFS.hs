{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.DeepSeq (NFData)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.ST (ST, runST)
import Control.Monad.State
  ( MonadState (get),
    State,
    evalState,
    modify,
    replicateM,
  )
import Criterion.Main
import Data.Functor ((<&>))
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics
import System.Random.MWC.Probability
import Test.Hspec (Expectation, shouldBe)

type Vertex = Int

type Table a = Vector a

type Graph = Table [Vertex]

data Tree a = Node a (Forest a) deriving (Eq, Show, Generic, NFData)

type Forest a = [Tree a]

dfs :: Graph -> Vertex -> Tree Vertex
dfs g = prune . generate g

generate :: Graph -> Vertex -> Tree Vertex
generate g u = let vs = g V.! u in Node u (generate g <$> vs)

prune :: Tree Vertex -> Tree Vertex
prune = flip evalState Set.empty . clip

clip :: Tree Vertex -> State (Set Vertex) (Tree Vertex)
clip (Node u ns) = do
  modify (Set.insert u)
  visited <- get
  let unvistedNodes = [n | n@(Node v _) <- ns, Set.notMember v visited]
  ns' <- mapM clip unvistedNodes
  return (Node u ns')

g1 :: Graph
g1 = V.fromList [[1, 2], [], [3, 4], [], [0]]

test :: Expectation
test = do
  dfs g1 0
    `shouldBe` Node 0 [Node 1 [], Node 2 [Node 3 [], Node 4 []]]

pruneST :: Tree Vertex -> Tree Vertex
pruneST t = runST $ newSTRef Set.empty >>= flip clipST t

clipST :: STRef s (Set Vertex) -> Tree Vertex -> ST s (Tree Vertex)
clipST visitedRef (Node u ns) = do
  modifySTRef visitedRef (Set.insert u)
  visited <- readSTRef visitedRef
  let unvistedNodes = [n | n@(Node v _) <- ns, Set.notMember v visited]
  ns' <- mapM (clipST visitedRef) unvistedNodes
  return (Node u ns')

dfsST :: Graph -> Vertex -> Tree Vertex
dfsST g = pruneST . generate g

type AdjacencyMatrix = Vector (Vector Bool)

randomAdjacencyMatrix ::
  PrimMonad m => Int -> Double -> Prob m AdjacencyMatrix
randomAdjacencyMatrix n z = V.fromList <$> replicateM n row
  where
    row = V.fromList <$> replicateM n (bernoulli p)
    p = z / fromIntegral n

fromAdjacencyMatrix :: AdjacencyMatrix -> Graph
fromAdjacencyMatrix mat = fmap findAll mat
  where
    findAll xs = [i | (i, x) <- zip [0 ..] (V.toList xs), x]

randomGraph :: PrimMonad m => Int -> Double -> Prob m Graph
randomGraph n = fmap fromAdjacencyMatrix . randomAdjacencyMatrix n

runBench :: (Graph -> Vertex -> Tree Vertex) -> Int -> Double -> IO (Tree Vertex)
runBench fun n z = create >>= sample (randomGraph n z) <&> \g -> fun g 0

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Using State"
        [ bench "random graph with 100 vertices, z=10" $ nfIO (runBench dfs 100 10),
          bench "random graph with 100 vertices, z=30" $ nfIO (runBench dfs 100 30),
          bench "random graph with 300 vertices, z=10" $ nfIO (runBench dfs 300 10),
          bench "random graph with 300 vertices, z=30" $ nfIO (runBench dfs 300 30)
        ],
      bgroup
        "Using ST"
        [ bench "random graph with 100 vertices, z=10" $ nfIO (runBench dfsST 100 10),
          bench "random graph with 100 vertices, z=30" $ nfIO (runBench dfsST 100 30),
          bench "random graph with 300 vertices, z=10" $ nfIO (runBench dfsST 300 10),
          bench "random graph with 300 vertices, z=30" $ nfIO (runBench dfsST 300 30)
        ]
    ]
