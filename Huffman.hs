module Huffman
    ( frequencies
    , encode
    , decode
    , Bit (..)
    ) where

import Data.List

data Bit = Z | O deriving (Eq, Show)

-- | Calculate symbol frequencies of a text.
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies x= sortBy (\(_,a) (_,b)->compare a b) (map (\x->(head x,length x)) (group (sort x)))

-- | Encode a sequence using the given frequencies.
encode :: Ord a => [(a, Int)] -> [a] -> Maybe [Bit]
encode lt as =if (length lt)>1 then encode' (build lt) as else Nothing

encode' tree (c:[])=Just (findMatch tree c)
encode' _ []=Just []
encode' tree (c:cs)=fmap ((findMatch tree c)++) (encode' tree cs)

-- | Decode a bit sequence using the given frequencies.
decode :: Ord a=>[(a, Int)] -> [Bit] -> Maybe [a]
decode lt bs=if (length lt)>1 then decode' (build lt) bs else Nothing

decode' tree []= Just []
decode' tree bl=do 
            (c',bl')<-extract tree bl 
            fmap (c':) (decode' tree bl')
--------------------------------------------------------------------------------------------------------------------------------------------------------
--Tree-relevant
data Tree a =Lf a Int|Node (Tree a) (Tree a) Int


extract :: Ord a=>(Tree a) ->[Bit]->Maybe (a,[Bit])
extract (Node _ _ _) []=Nothing
extract (Lf c _) bl=Just (c,bl)
extract (Node a b _) (bit:bits)=if bit== Z then extract a bits else extract b bits

findMatch  :: Ord a=>(Tree a) ->a->[Bit]
findMatch (Lf c _) c'=[]
findMatch (Node a b _) c'=if ext a c' then Z:(findMatch a c') else O:(findMatch b c')

ext :: Ord a=>Tree a->a->Bool
ext (Lf c _) c'=(c==c')
ext (Node a b _) c'=(ext a c')||(ext b c')


build x=build' (map (\(a,wei)->Lf a wei) x)
build' :: Ord a=>[Tree a]->Tree a
build' (x:[])=x
build' xs=let xs'=sort xs in build' ((comb (take 2 xs')):(drop 2 xs'))
comb ((a@(Lf _ wei)):(b@(Lf _ wei')):[])=Node b a (wei+wei')
comb ((a@(Node _ _ wei)):(b@(Lf _ wei')):[])=Node b a (wei+wei')
comb ((a@(Node _ _ wei)):(b@(Node _ _ wei')):[])=Node b a (wei+wei')
comb ((a@(Lf _  wei)):(b@(Node _ _ wei')):[])=Node b a (wei+wei')

instance Eq (Tree a) where
    (==) _ _ =False
instance Ord (Tree a) where
    (>) (Lf _ a) (Lf _ b)=(a>b)
    (>) (Lf _ a) (Node _ _ b)=(a>b)
    (>) (Node _ _ a) (Lf _ b)=(a>b)
    (>) (Node _ _ a) (Node _ _ b)=(a>b)
    (<=) x y=not ((>) x y)
