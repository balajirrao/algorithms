{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Dijkstra where

import Control.Monad (forM, replicateM)
import Control.Monad.State.Lazy (MonadState (get, put), State, evalState, forM_)
import Data.Foldable (foldrM)
import Data.Graph.Inductive.Arbitrary ()
import Data.Graph.Inductive.Graph (Graph, Node)
import qualified Data.Graph.Inductive.Graph as FGL
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (LRTree, spTree)
import Data.List (foldl', nub, sortBy)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import qualified Data.Set as S
import Debug.Trace
import GHC.Natural (Natural, intToNatural)
import Test.QuickCheck (Arbitrary (arbitrary), quickCheck, suchThat, verboseCheck)

data DijkstraState = DijkstraState
  { unvisitedNodes :: S.Set Node,
    tentativeDistances :: M.Map Node Int
  }
  deriving (Show)

setTentativeDistance :: Node -> Int -> State DijkstraState ()
setTentativeDistance n d = do
  st@DijkstraState {tentativeDistances} <- get
  let st' = st {tentativeDistances = M.insert n d tentativeDistances}
  put st'

selectNextNode :: State DijkstraState (Maybe (Node, Int))
selectNextNode = do
  st@DijkstraState {..} <- get
  let univisitedTentativeDistances =
        M.toList $ M.filterWithKey (\node _ -> node `S.member` unvisitedNodes) tentativeDistances

  let res = sortBy (comparing snd) univisitedTentativeDistances
  case res of
    [] -> pure Nothing
    (x : _) -> pure $ pure x

markNodeVisited :: Graph gr => gr a b -> Node -> State DijkstraState ()
markNodeVisited graph node = do
  st@DijkstraState {unvisitedNodes} <- get
  let newUnivisitedNodes = S.delete node unvisitedNodes
  put $ st {unvisitedNodes = newUnivisitedNodes}

doDijkstra :: Graph gr => gr a b -> Node -> Int -> State DijkstraState ()
doDijkstra graph node nodeTentativeDistance = do
  st@DijkstraState {..} <- get
  let unvisitedNeighbors = S.intersection unvisitedNodes (S.fromList $ FGL.suc graph node)

  forM_ unvisitedNeighbors $ \nei -> do
    let td = M.lookup nei tentativeDistances
        newTd = maybe (1 + nodeTentativeDistance) (min (1 + nodeTentativeDistance)) td
    setTentativeDistance nei newTd

  markNodeVisited graph node

  let loop = do
        next <- selectNextNode
        case next of
          Nothing -> pure ()
          (Just (n', d')) -> doDijkstra graph n' d' >> loop

  loop

reconstructedSPTree :: Graph gr => gr a b -> Node -> Int -> State DijkstraState [[Node]]
reconstructedSPTree graph node dist = do
  st@DijkstraState {..} <- get
  let sucNodes = S.fromList $ FGL.suc graph node
  let nextNodes = M.keys $ M.filterWithKey (\n d -> d == dist + 1 && n `S.member` sucNodes) tentativeDistances
  subTrees <- concat <$> forM nextNodes (\n -> reconstructedSPTree graph n (dist + 1))
  pure $ ([node] :) $ (node :) <$> subTrees

dijkstra :: Graph gr => gr a b -> Node -> [[Node]]
dijkstra graph node
  | node `elem` FGL.nodes graph =
    reverse
      <$> evalState
        (doDijkstra graph node 0 >> reconstructedSPTree graph node 0)
        (DijkstraState (S.fromList $ FGL.nodes graph) (M.singleton node 0))
  | otherwise = []

dijkstraGr :: Gr () Natural -> Node -> [[Node]]
dijkstraGr = dijkstra @Gr

prop_shortestPath :: Graph gr => gr () Natural -> Bool
prop_shortestPath graph = S.fromList (dijkstra graph node) == S.fromList (processSpTree (spTree node graph))
  where
    node = foldr const 0 (FGL.nodes graph)
    processSpTree :: LRTree a -> [[Node]]
    processSpTree paths = nub $ (fst <$>) <$> (FGL.unLPath <$> paths)

instance Arbitrary Natural where
  arbitrary = intToNatural <$> (arbitrary `suchThat` (>= 0))

main :: IO ()
main = verboseCheck (prop_shortestPath @Gr)
