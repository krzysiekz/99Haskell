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

data Graph a = Graph [a] [(a,a)] deriving (Show, Eq)
data Adj a = Adj [(a, [a])] deriving (Show, Eq)
data Friendly a = Edge [(a, a)] deriving (Show, Eq)

graphToAdj :: (Eq a) => Graph a -> Adj a
graphToAdj (Graph v l) = Adj $ map (\e -> (e, map (\(x,y) -> (if x == e then y else x)) (filter (\(x,y) -> x == e || y == e) l))) v

-- graphToFriendly :: (Eq a) => Graph a -> Friendly a
-- graphToFriendly (Graph v l) = Edge $ concatMap (\e -> map (\(_,y) -> (e, y)) (filter (\(x,_) -> x == e ) l)) v

adjToGraph :: (Eq a) => Adj a -> Graph a
adjToGraph (Adj c) = Graph v l
    where v = map fst c
          l = foldl removeDuplicates [] $ concatMap (\(x,y) -> map (\e -> (x, e)) y) c
          removeDuplicates acc e@(x,y) = if e `elem` acc || (y,x) `elem` acc then acc else acc ++ [e]



