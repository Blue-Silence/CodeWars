module RailFenceCipher.Kata (encode,decode) where
import Data.List
encode :: [a] -> Int -> [a]
encode [] _=[]
encode  (a:as) n=let ((xs,y,zs),_)=downE (([],a:[],(replicate (n-1) [])),as) in concat ((reverse (map reverse xs)) ++ (map reverse (y:zs)))

decode :: [a] -> Int -> [a]
decode [] _=[]
decode lt n=let len=length lt in map (\(ValP _ v)->v) (sort (zipWith (\ n v->ValP n v) ((encode [1,2..len] n)::[Int]) lt))

upE :: (([[a]],[a],[[a]]),[a])->(([[a]],[a],[[a]]),[a])
upE a@(x,[])=a
upE a@(([],now,down),str)=downE a
upE ((x:xs,y,z),a:as)=upE ((xs,a:x,y:z),as)

downE :: (([[a]],[a],[[a]]),[a])->(([[a]],[a],[[a]]),[a])
downE a@(x,[])=a
downE a@((up,now,[]),str)=upE a
downE ((xs,y,z:zs),a:as)=downE ((y:xs,a:z,zs),as)


data ValP a=ValP Int a deriving Show
instance Eq (ValP a) where
    (==) (ValP a _) (ValP b _) =a==b
instance Ord (ValP a) where
    compare (ValP a _) (ValP b _)=compare a b
