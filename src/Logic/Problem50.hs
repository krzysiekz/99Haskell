module Logic.Problem50 where

-- (***) Huffman codes.
--
-- We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms.
-- Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)].
-- Our objective is to construct a list hc(S,C) terms, where C is the Huffman code word for the symbol S.
-- In our example, the result could be Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.].
-- The task shall be performed by the predicate huffman/2 defined as follows:
--
-- % huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs
-- Example in Haskell:
--
-- *Exercises> huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
-- [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]

import qualified Data.List as List

type Character = Maybe Char
type Frequency = Int

data HuffmanTree = Empty | Node Character Frequency HuffmanTree HuffmanTree deriving (Show)

huffman :: [(Char, Int)] -> [(Char, String)]
huffman input = map (\(char, _) -> (char, init (findPath tree char))) input
    where tree = head $ huffmanTree $ map (\(char, frequency) -> Node (Just char) frequency Empty Empty) input

huffmanTree :: [HuffmanTree] -> [HuffmanTree]
huffmanTree forest@[x] = forest
huffmanTree (x:y:xs) = huffmanTree (sortTree (createNode x y:xs))
    where createNode a@(Node _ f _ _) b@(Node _ f2 _ _) = Node Nothing (f + f2) a b

sortTree :: [HuffmanTree] -> [HuffmanTree]
sortTree = List.sortBy (\(Node _ f _ _) (Node _ f2 _ _) -> compare f f2)

findPath :: HuffmanTree -> Char -> String
findPath Empty _ = []
findPath (Node char _ left right) target
    | char == Just target = ['x']
    | not (null leftResult) = '0':leftResult
    | not (null rightResult) = '1' : rightResult
    | otherwise = []
        where leftResult = findPath left target
              rightResult = findPath right target
