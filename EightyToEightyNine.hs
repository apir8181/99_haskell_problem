import Data.List

data Graph a = Graph { getNodes :: [a], 
                       getEdges :: [(a, a)] 
               } deriving (Show, Eq)

data AdjGraph a = AdjGraph [(a, [a])]  deriving (Show, Eq)

data HumanGraph a = HumanGraph [(a, a)] deriving (Eq)

instance (Show a) => Show (HumanGraph a) where
  show (HumanGraph xs) = show $ map (\(x, y) -> show x ++ "-" ++ show y) xs

-- 80

-- graph Conversion
graphToAdjGraph :: (Eq a) => Graph a -> AdjGraph a
graphToAdjGraph (Graph nodes edges) = 
  let nedges = map (\node -> (node, getNodeEdges node)) nodes
  in AdjGraph nedges
  where getNodeEdges node = concat $ map (\(x, y) -> 
                                           if x /= node && y /= node
                                             then []
                                             else if x == node
                                                  then [y]
                                                  else [x])
                                     edges

  
graphToHumanGraph :: Graph a -> HumanGraph a
graphToHumanGraph (Graph nodes edges) = HumanGraph edges

-- adjGraph Conversion
adjGraphDupEdges :: AdjGraph a -> [(a, a)]
adjGraphDupEdges (AdjGraph nodeEdges) =
  concat $ map (\(node, nextNodes) -> map (\next -> (node, next)) nextNodes)
           nodeEdges

adjGraphToGraph :: (Ord a) => AdjGraph a -> Graph a
adjGraphToGraph adjGraph@(AdjGraph nodeEdges) =
  let nodes = map (\(node, nextNodes) -> node) nodeEdges
      dupEdges = adjGraphDupEdges adjGraph
      edges = filter (\(x, y) -> x < y) dupEdges
  in Graph nodes edges

adjGraphToHumanGraph :: (Ord a) => AdjGraph a -> HumanGraph a
adjGraphToHumanGraph adjGraph = 
  let dupEdges = adjGraphDupEdges adjGraph
      edges = filter (\(x, y) -> x < y) dupEdges
  in HumanGraph edges

-- HumanGraph Conversion
humanGraphToGraph :: (Ord a) => HumanGraph a -> Graph a
humanGraphToGraph (HumanGraph edges) =
  let dupNodes = edges >>= (\(x, y) -> [x, y])
      nodes = sort $ nub dupNodes
  in Graph nodes edges

humanGraphToAdjGraph :: (Ord a) => HumanGraph a -> AdjGraph a
humanGraphToAdjGraph humanGraph =
  let graph = humanGraphToGraph humanGraph
  in graphToAdjGraph graph


graph1 = Graph ['b', 'c', 'd', 'f', 'g', 'h', 'k']
               [('b','c'), ('b','f'), ('c','f'), ('f','k'), ('g','h')]

adjGraph1 = AdjGraph [('b',"cf"), ('c',"bf"), ('d',""), ('f',"bck"),
                      ('g',"h"), ('h',"g"), ('k',"f")]

humanGraph1 = HumanGraph [('b','c'), ('b','f'), ('c','f'), 
                          ('f','k'), ('g','h')]

-- 81 for graph, not bigraph
adjGraphNodeNexts :: (Eq a) => AdjGraph a -> a -> [a]
adjGraphNodeNexts (AdjGraph nodeEdges) e = helper nodeEdges
  where helper (x:xs) = if fst x == e then (snd x) else helper xs

adjGraphPaths :: (Ord a) => AdjGraph a -> a -> a -> [[a]]
adjGraphPaths adjGraph@(AdjGraph nodeEdges) src dst =
    case do { src `lookup` nodeEdges >> dst `lookup` nodeEdges } of
      Nothing -> []
      otherwise -> helper [[src]] []
  where helper [] acc = acc
        helper (path:paths) acc = 
          if last path == dst then helper paths (path:acc)
          else 
            let nowElem = last path
                nexts = adjGraphNodeNexts adjGraph nowElem
                validNexts = filter (\next -> not $ next `elem` path) nexts
                newPaths = map (\next -> path ++ [next]) validNexts
            in helper (paths ++ newPaths) acc 

humanGraphPaths :: (Ord a) => HumanGraph a -> a -> a -> [[a]]
humanGraphPaths humanGraph src dst =
  let adjGraph = humanGraphToAdjGraph humanGraph
  in adjGraphPaths adjGraph src dst

humanGraph2 = HumanGraph [(1,2), (2,3), (1,3), (3,4), (4,2),(5,6)]
