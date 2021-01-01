{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Dijkstra where

import Control.Monad (forM, replicateM)
import Control.Monad.State.Strict (MonadState (get, put), State, evalState, forM_)
import Data.Foldable (foldrM)
import Data.Graph.Inductive.Arbitrary ()
import Data.Graph.Inductive.Graph (Graph, Node)
import qualified Data.Graph.Inductive.Graph as FGL
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (LRTree, spLength, spTree)
import Data.List (foldl', nub, sortBy, sortOn)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import qualified Data.Set as S
import GHC.Natural (Natural, intToNatural)
import Test.QuickCheck (Arbitrary (arbitrary), elements, quickCheck, quickCheckWith, stdArgs, suchThat, verboseCheck)
import Test.QuickCheck.Test (Args (maxSuccess))

data DijkstraState b = DijkstraState
  { unvisitedNodes :: S.Set Node,
    tentativeDistances :: M.Map Node b
  }
  deriving (Show)

setTentativeDistance :: Ord b => Node -> b -> State (DijkstraState b) ()
setTentativeDistance n d = do
  st@DijkstraState {tentativeDistances} <- get
  let st' = st {tentativeDistances = M.insertWith min n d tentativeDistances}
  put st'

selectNextNode :: (Ord b) => State (DijkstraState b) (Maybe (Node, b))
selectNextNode = do
  st@DijkstraState {..} <- get
  let univisitedTentativeDistances =
        M.toList $ M.filterWithKey (\node _ -> node `S.member` unvisitedNodes) tentativeDistances

  let res = sortBy (comparing snd) univisitedTentativeDistances
  case res of
    [] -> pure Nothing
    (x : _) -> pure $ pure x

markNodeVisited :: Graph gr => gr a b -> Node -> State (DijkstraState b) ()
markNodeVisited graph node = do
  st@DijkstraState {unvisitedNodes} <- get
  let newUnivisitedNodes = S.delete node unvisitedNodes
  put $ st {unvisitedNodes = newUnivisitedNodes}

doDijkstra :: (Graph gr, Real b) => gr a b -> Node -> b -> State (DijkstraState b) ()
doDijkstra graph node nodeTentativeDistance = do
  st@DijkstraState {..} <- get
  let unvisitedNeighbors = M.toList $ M.fromListWith min $ filter (\(n, b) -> n `S.member` unvisitedNodes) $ FGL.lsuc graph node

  forM_ unvisitedNeighbors $ \(nei, b) -> do
    let td = M.lookup nei tentativeDistances
        newTd = maybe (b + nodeTentativeDistance) (min (b + nodeTentativeDistance)) td
    setTentativeDistance nei newTd

  markNodeVisited graph node

  let loop = do
        next <- selectNextNode
        case next of
          Nothing -> pure ()
          (Just (n', d')) -> doDijkstra graph n' d' >> loop

  loop

reconstructedSPTree ::
  forall gr a b.
  (Real b, Graph gr, Show b) =>
  gr a b ->
  Node ->
  b ->
  State (DijkstraState b) [[(Node, b)]]
reconstructedSPTree graph node dist = do
  markNodeVisited graph node
  st@DijkstraState {..} <- get
  let sucNodes = M.filterWithKey (\n _ -> S.member n unvisitedNodes) $ M.fromListWith min $ FGL.lsuc graph node

  let filterFunc n b = case M.lookup n tentativeDistances of
        Nothing -> False
        (Just d) -> d == dist + b

  let nextNodes = sortOn snd $ M.toList $ M.filterWithKey filterFunc sucNodes
  subTrees <- concat . catMaybes <$> forM nextNodes reconstructIfUnvisited
  pure $ ([(node, dist)] :) $ ((node, dist) :) <$> subTrees
  where
    reconstructIfUnvisited :: (Node, b) -> State (DijkstraState b) (Maybe [[(Node, b)]])
    reconstructIfUnvisited (n, b) = do
      st@DijkstraState {..} <- get
      if S.member n unvisitedNodes then Just <$> reconstructedSPTree graph n (dist + b) else pure Nothing

dijkstra :: (Real b, Graph gr, Show b) => gr a b -> Node -> [[(Node, b)]]
dijkstra graph node
  | node `elem` FGL.nodes graph =
    reverse
      <$> evalState
        (doDijkstra graph node 0 >> reset >> reconstructedSPTree graph node 0)
        (DijkstraState (S.fromList $ FGL.nodes graph) (M.singleton node 0))
  | otherwise = []
  where
    reset :: State (DijkstraState b) ()
    reset = do
      st@DijkstraState {..} <- get
      put st {unvisitedNodes = S.fromList $ FGL.nodes graph}

dijkstraGr :: Gr () Natural -> Node -> [[(Node, Natural)]]
dijkstraGr = dijkstra

prop_shortestPath :: Graph gr => GraphNodePair (gr () Natural) -> Bool
prop_shortestPath (GraphNodePair graph node) =
  length a == length b && and (computeLength <$> a)
  where
    a = dijkstra graph node
    b = spTree node graph
    computeLength [] = error "imposible"
    computeLength p = case spLength (fst . last $ p) (fst . head $ p) graph of
      Nothing -> False
      Just n -> snd (head p) == n

data GraphNodePair graph = GraphNodePair graph Node deriving (Show)

instance
  (Graph g, Arbitrary (g a b)) =>
  Arbitrary (GraphNodePair (g a b))
  where
  arbitrary = do
    g <- arbitrary `suchThat` \g -> not . null $ FGL.nodes g
    n <- elements (FGL.nodes g)
    pure $ GraphNodePair g n

instance Arbitrary Natural where
  arbitrary = intToNatural <$> (arbitrary `suchThat` (> 0))

main :: IO ()
main = quickCheckWith (stdArgs {maxSuccess = 10000}) (prop_shortestPath @Gr)
