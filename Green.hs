--count' cout n=if (div n 10)==0 then (cout+1) else count' (cout+1) (div n 10)
--count n=count' 0 n


module Green where
import Data.List
green :: Int -> Integer
green n = (gen 1)!!(n-1)

gen :: Int->[Integer]
gen 1=[1,5,6]++(gen 2)
gen n=(sort (gen' n))++(gen (n+1)) 


gen' n=(gen5 n)++(gen6 n)
gen5 n=do
    x<-[1,2,3]
    let c=div (x*(10^n)) 4
    if ((lst' c)==5)&&((mod (c*(c-1)) (10^n)))==0 then [c] else []
gen6 n=do
    let x=3
    let c=div (x*(10^n)) 5
    if ((lst' c)==6)&&((mod (c*(c-1)) (10^n)))==0 then [c] else []
lst' n=mod n 10