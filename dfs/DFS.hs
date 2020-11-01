{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.DeepSeq (NFData)
import Control.Monad (forM_, replicateM)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.ST (ST, runST)
import Control.Monad.State
  ( MonadState (get),
    State,
    evalState,
    modify,
  )
import Criterion.Main (bench, bgroup, defaultMain, nfIO)
import Data.Functor ((<&>))
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import System.Random.MWC.Probability
  ( Prob (sample),
    bernoulli,
    create,
  )
import Test.Hspec (describe, hspec, it, shouldBe)
import Text.Printf (printf)

type Vertex = Int

type Table a = Vector a

type Graph = Table [Vertex]

data Tree a = Node a (Forest a) deriving (Eq, Show, Generic, NFData)

type Forest a = [Tree a]

dfs :: (Tree Vertex -> Tree Vertex) -> Graph -> Vertex -> Tree Vertex
dfs pruneImpl g = pruneImpl . generate g

generate :: Graph -> Vertex -> Tree Vertex
generate g u = let vs = g V.! u in Node u (generate g <$> vs)

prune :: Tree Vertex -> Tree Vertex
prune = fst . flip clip Set.empty

clip :: Tree Vertex -> Set Vertex -> (Tree Vertex, Set Vertex)
clip (Node l xs) visited = (Node l xs1, visited1)
  where
    (xs1, visited1) =
      foldr
        ( \x'@(Node l' _) z@(xs', visited') ->
            if l' `Set.notMember` visited'
              then let (x'', visited'') = clip x' visited' in (x'' : xs', visited'')
              else z
        )
        ([], Set.insert l visited)
        xs

pruneState :: Tree Vertex -> Tree Vertex
pruneState = flip evalState Set.empty . clipState

clipState :: Tree Vertex -> State (Set Vertex) (Tree Vertex)
clipState (Node l xs) = do
  modify (Set.insert l)
  visited <- get
  let unvistedNodes = filter (\(Node l' _) -> l' `Set.notMember` visited) xs
  xs' <- mapM clipState unvistedNodes
  return (Node l xs')

pruneST :: Tree Vertex -> Tree Vertex
pruneST t = runST $ newSTRef Set.empty >>= flip clipST t

clipST :: STRef s (Set Vertex) -> Tree Vertex -> ST s (Tree Vertex)
clipST visitedRef (Node l xs) = do
  modifySTRef visitedRef (Set.insert l)
  visited <- readSTRef visitedRef
  let unvistedNodes = filter (\(Node l' _) -> l' `Set.notMember` visited) xs
  xs' <- mapM (clipST visitedRef) unvistedNodes
  return (Node l xs')

dfsIO :: (Tree Vertex -> IO (Tree Vertex)) -> Graph -> Vertex -> IO (Tree Vertex)
dfsIO pruneImpl g = pruneImpl . generate g

pruneIO :: Tree Vertex -> IO (Tree Vertex)
pruneIO t = newIORef Set.empty >>= flip clipIO t

clipIO :: IORef (Set Vertex) -> Tree Vertex -> IO (Tree Vertex)
clipIO visitedRef (Node l xs) = do
  modifyIORef visitedRef (Set.insert l)
  visited <- readIORef visitedRef
  let unvistedNodes = filter (\(Node l' _) -> l' `Set.notMember` visited) xs
  ns' <- mapM (clipIO visitedRef) unvistedNodes
  return (Node l ns')

g1, g2, g3 :: Graph
g1 = V.fromList [[1, 2], [], [3, 4], [], [0]]
g2 = V.fromList [[1], [0]]
g3 = V.fromList [[1], [2], [0]]

test :: IO ()
test = hspec $
  forM_ [("foldr", prune), ("State monad", pruneState), ("ST monad", pruneST)] $ \(name, impl) ->
    describe name $
      it "should handle simple case" $ do
        dfs impl g1 0 `shouldBe` Node 0 [Node 1 [], Node 2 [Node 3 [], Node 4 []]]
        dfs impl g1 1 `shouldBe` Node 1 []
        dfs impl g1 2 `shouldBe` Node 2 [Node 3 [], Node 4 [Node 0 [Node 1 []]]]
        dfs impl g1 3 `shouldBe` Node 3 []
        dfs impl g1 4 `shouldBe` Node 4 [Node 0 [Node 1 [], Node 2 [Node 3 []]]]

type AdjacencyMatrix = Vector (Vector Bool)

randomAdjacencyMatrix ::
  PrimMonad m => Int -> Double -> Prob m AdjacencyMatrix
randomAdjacencyMatrix n p = V.fromList <$> replicateM n row
  where
    row = V.fromList <$> replicateM n (bernoulli p)

fromAdjacencyMatrix :: AdjacencyMatrix -> Graph
fromAdjacencyMatrix mat = fmap findAll mat
  where
    findAll xs = [i | (i, x) <- zip [0 ..] (V.toList xs), x]

randomGraph :: PrimMonad m => Int -> Double -> Prob m Graph
randomGraph n = fmap fromAdjacencyMatrix . randomAdjacencyMatrix n

runBench :: (Graph -> Vertex -> Tree Vertex) -> Int -> Double -> IO (Tree Vertex)
runBench fun n z = create >>= sample (randomGraph n z) <&> \g -> fun g 0

runBenchIO :: (Graph -> Vertex -> IO (Tree Vertex)) -> Int -> Double -> IO (Tree Vertex)
runBenchIO fun n z = create >>= sample (randomGraph n z) >>= \g -> fun g 0

main :: IO ()
main =
  defaultMain
    [ bgroup
        (printf "N=%d, p=%0.2f" size p)
        ( [ bench name . nfIO $ runBench (dfs impl) size p
            | (name, impl) <- [("foldr", prune), ("State monad", pruneState), ("ST monad", pruneST)]
          ]
            ++ [bench "IO monad" . nfIO $ runBenchIO (dfsIO pruneIO) size p]
        )
      | size <- [100, 300],
        p <- [0.1, 0.3, 0.5]
    ]
