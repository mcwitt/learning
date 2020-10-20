{-# LANGUAGE RankNTypes #-}

module DFS where

import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.State
import Data.Array
  ( Array,
    Ix,
    array,
    bounds,
    (!),
  )
import Data.STRef
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random.MWC
import Test.Hspec

type Vertex = Char

type Table a = Array Vertex a

type Graph = Table [Vertex]

data Tree a = Node a (Forest a) deriving (Eq, Show)

type Forest a = [Tree a]

dfs :: Graph -> Vertex -> Tree Vertex
dfs g = prune . generate g

generate :: Graph -> Vertex -> Tree Vertex
generate g u = let vs = g ! u in Node u (generate g <$> vs)

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
g1 =
  array
    ('A', 'E')
    [('A', ['B', 'C']), ('B', []), ('C', ['D', 'E']), ('D', []), ('E', ['A'])]

test :: Expectation
test = do
  dfs g1 'A' `shouldBe` Node 'A' [Node 'B' [], Node 'C' [Node 'D' [], Node 'E' []]]

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

-------------------------------------------------------------------------------

sample :: (PrimMonad m, Ix i, UniformRange i) => Int -> Array i a -> Gen (PrimState m) -> m [a]
sample n xs g = fmap (xs !) <$> replicateM n (uniformRM (bounds xs) g)

randomGraph :: PrimMonad m => [Vertex] -> Gen (PrimState m) -> m Graph
randomGraph vs g = return $ array (minimum vs, maximum vs) []
  -- where randomNeighbors = [sample n vs | n <-

