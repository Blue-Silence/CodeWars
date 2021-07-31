module InfiniteDigitalString where
import Data.List
import Data.Array
findPosition :: [Char] -> Integer
findPosition str = (match ltC str)-(toEnum (length str))

lt=(1:(map (1+) lt))
ltC=concat (map show lt)

data Node=Node Char Char | End deriving Show

constrt :: [Char]->[Node]
constrt a@(x:xs)=(map (Node x) a)++[End]

--         stream   len  nodes position stats
forward :: [Char]->(Int,Array Int Node)->Integer->[Int]->Integer
forward (l:lt) (len,nodes) poi stats=let (max:_)=stats in
                                                        if max== len then poi
                                                                else let stats'=(reverse . (map head) . group . sort . (0:)) (concatMap (step nodes l) stats) in forward lt (len,nodes) (poi+1) stats'

step :: Array Int Node->Char->Int->[Int]
step nodes c stat
    |head==c=if c==next then [1,stat+1] else [1]
    |c==next=[stat+1]
    |otherwise=[]
        where (Node head next)=nodes ! stat

match :: [Char]->[Char]->Integer
match stream target =let nodes=cst target
                         len=length target
                         in forward stream (len,nodes) 0 [0]

cst :: [Char] -> Array Int Node
cst str=(listArray (0,length str). constrt) str