module Fibonacci where

fib :: Integer -> Integer

spiL :: [(Integer, Integer)]
spiL =  (1,0) : (0,1) : zipWith merge spiL (drop 1 spiL)
merge :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
merge (a,b) (c,d)=(a+c,b+d)

offList :: [(Integer, Integer)]
offList=take 10000 spiL

fragLen :: Integer
fragLen=10000

data Head=Head Integer Integer
 deriving Show
fragList :: [Head]
fragList = Head 1 1 : constrHeads 1 1

constrHeads fst snd=let (x1,y1)=(pick offList  (fragLen-2))
                        (x2,y2)=(pick offList  (fragLen-1))
                        x=fst*x1+snd*y1
                        y=fst*x2+snd*y2
                        fst'= (x+y)
                        snd'=(x+y+y)
                    in Head fst' snd' : constrHeads fst' snd' 

fib' :: Integer -> Integer
fib' num=let frag=div num fragLen
             Head fst snd=pick fragList  frag
             offset=mod num fragLen
             (x,y)=(pick offList  (offset-1))
      in fst*x+snd*y
        
fib num 
    |num>0=fib' num 
    |num<0=if (mod num 2)==0 then (-(fib' (-num))) else (fib' (-num))
    |otherwise=0

pick (x:_) 0 =x
pick (_:xs) n =pick xs (n-1)