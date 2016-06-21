module Graphs.Problem80 where

-- (***) Conversions
--
-- Write predicates to convert between the different graph representations. With these predicates,
-- all representations are equivalent; i.e. for the following problems you can always pick freely the most convenient form.
-- The reason this problem is rated (***) is not because it's particularly difficult, but because it's a
-- lot of work to deal with all the special cases.
--
-- Example in Haskell:
--
-- graphToAdj (Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')])
-- Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]

import Data.List

data Graph a = Graph [a] [(a,a)] deriving (Show, Eq)
data Adj a = Adj [(a, [a])] deriving (Show, Eq)
data Friendly a = Edge [(a, a)] deriving (Show, Eq)

graphToAdj :: (Eq a) => Graph a -> Adj a
graphToAdj (Graph v l) = Adj $ map (\e -> (e, map (\(x,y) -> (if x == e then y else x)) (filter (\(x,y) -> x == e || y == e) l))) v

graphToFriendly :: (Eq a) => Graph a -> Friendly a
graphToFriendly (Graph v l) = Edge $ l ++ findSingleNodes v l
    where findSingleNodes v l = concatMap (\e -> [(e, e)|isSingle e l]) v
          isSingle e = foldl (\acc (x, y) -> (acc && (x /= e && y /= e))) True


adjToGraph :: (Eq a) => Adj a -> Graph a
adjToGraph (Adj c) = Graph v l
    where v = map fst c
          l = foldl removeDuplicates [] $ concatMap (\(x,y) -> map (\e -> (x, e)) y) c
          removeDuplicates acc e@(x,y) = if e `elem` acc || (y,x) `elem` acc then acc else acc ++ [e]

adjToFriendly :: (Eq a) => Adj a -> Friendly a
adjToFriendly (Adj c) = Edge $ concatMap (\(x,y) -> if null y then [(x, x)] else map (\e -> (x, e)) y) c

friendlyToGraph :: (Eq a) => Friendly a -> Graph a
friendlyToGraph (Edge e) = Graph vertexes edges
    where vertexes = nub $ foldl (\acc (x,y) -> x:y:acc) [] e
          edges = foldl (\acc se@(x,y) -> if x == y || se `elem` acc || (y,x) `elem` acc then acc else se:acc) [] e

friendlyToAdj :: (Eq a) => Friendly a -> Adj a
friendlyToAdj (Edge e) = Adj edges
    where vertexes = nub $ foldl (\acc (x,y) -> x:y:acc) [] e
          edges = map (\v -> (v, findMatching v e)) vertexes
          findMatching v e = nub [if x == v then y else x | (x,y) <- e, x == v || y == v, x /= y]



