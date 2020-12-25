{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Dijkstra where

import Control.Monad.State (MonadState (get, put), State, evalState, forM_)
import Data.Graph.Inductive.Arbitrary ()
import Data.Graph.Inductive.Graph (Graph, Node)
import qualified Data.Graph.Inductive.Graph as FGL
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (LRTree, spTree)
import Data.List (foldl', sortBy)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import qualified Data.Set as S
import GHC.Natural (Natural, intToNatural)
import Test.QuickCheck (Arbitrary (arbitrary), quickCheck, suchThat, verboseCheck)

data DijkstraState = DijkstraState
  { unvisitedNodes :: S.Set Node,
    tentativeDistances :: M.Map Node Int
  }

setTentativeDistance :: Node -> Int -> State DijkstraState ()
setTentativeDistance n d = do
  st@DijkstraState {tentativeDistances} <- get
  put $ st {tentativeDistances = M.insert n d tentativeDistances}

selectNextNode :: State DijkstraState (Maybe (Node, Int))
selectNextNode = do
  st@DijkstraState {..} <- get

  let univisitedTentativeDistances =
        M.toList $ M.filterWithKey (\node _ -> node `S.member` unvisitedNodes) tentativeDistances

  case sortBy (comparing snd) univisitedTentativeDistances of
    [] -> pure Nothing
    (x : _) -> pure $ pure x

doDijkstra :: Graph gr => gr a b -> Node -> Int -> State DijkstraState Int
doDijkstra graph node nodeTentativeDistance = do
  st@DijkstraState {..} <- get
  let unvisitedNeighbors = S.intersection unvisitedNodes (S.fromList $ FGL.neighbors graph node)

  forM_ unvisitedNeighbors $ \nei -> do
    let td = M.lookup nei tentativeDistances
        newTd = maybe (1 + nodeTentativeDistance) (min (1 + nodeTentativeDistance)) td
    setTentativeDistance nei newTd

  let newUnivisitedNodes = S.delete node unvisitedNodes
  put $ st {unvisitedNodes = newUnivisitedNodes}

  next <- selectNextNode
  case next of
    Nothing -> pure nodeTentativeDistance
    (Just (n', d')) -> doDijkstra graph n' d'

dijkstra :: Graph gr => gr a b -> Node -> Int
dijkstra graph node = evalState (doDijkstra graph node 0) (DijkstraState mempty mempty)

prop_shortestPath :: Graph gr => gr () Natural -> Bool
prop_shortestPath graph = dijkstra graph node == processSpTree (spTree node graph)
  where
    node = foldr const 0 (FGL.nodes graph)
    processSpTree :: LRTree a -> Int
    processSpTree paths =
      let pathLengths = length . FGL.unLPath <$> paths
       in case pathLengths of
            [] -> 0
            (x : xs) -> foldl' min x xs - 1

instance Arbitrary Natural where
  arbitrary = intToNatural <$> (arbitrary `suchThat` (>= 0))

main :: IO ()
main = verboseCheck (prop_shortestPath @Gr)
