module InfiniteDigitalString where
import Data.List
findPosition :: [Char] -> Integer
findPosition str = (match ltC str)-(toEnum (length str))

lt=(1:(map (1+) lt))
ltC=concat (map show lt)

data Node=Node Char Char | End deriving Show

constrt :: [Char]->[Node]
constrt a@(x:xs)=(map (Node x) a)++[End]


--         stream   len  nodes position stats
forward :: [Char]->(Int,[Node])->Integer->[Int]->Integer
forward (l:lt) (len,nodes) poi stats=let (max:_)=stats in
                                                        if max== len then poi
                                                                else let stats'=(reverse . (map head) . group . sort)(concatMap (step nodes l) stats) in forward lt (len,nodes) (poi+1) stats'

step :: [Node]->Char->Int->[Int]
step nodes@((Node head _):_) c stat
    |head==c=if c==next then [1,stat+1] else [1]
    |c==next=[0,stat+1]
    |otherwise=[0]
        where (Node _ next)=nodes !! stat

match :: [Char]->[Char]->Integer
match stream target =let nodes=constrt target
                         len=length target
                         in forward stream (len,nodes) 0 [0]