module MultNumAsStrings where

import Data.Char (intToDigit, digitToInt)

multiply x y=let rd=map digitToInt in
                    let sh=map intToDigit  in
                        let lt1 =(rd x) in
                            let lt2=(rd y) in
                                mvZ (sh (rebuild' (multiply' lt1 lt2 [0])))

multiply' :: [Int]->[Int]->[Int]->[Int]
multiply' (x:[]) ys bf=rebuild (plus (bf++[0]) (map (x*) ys))
multiply' (x:xs) ys bf=rebuild (multiply' xs ys (plus (bf++[0]) (map (x*) ys)))

rebuild :: [Int]->[Int]
rebuild (x:[])=(x:[])
rebuild (x:xs)=let (y':ys')=rebuild xs in (x+(div y' 10)):(mod y' 10):ys'

plus x y= (let n1=length x in
                    let n2=length y in
                        if n1>n2 then (take (n1-n2) x)++(zipWith (+) (drop (n1-n2) x) y) else (take (n2-n1) y)++(zipWith (+) (drop (n2-n1) y) x))
mvZ ('0':[])=('0':[])
mvZ ('0':xs)=mvZ xs
mvZ xs=xs
rebuild' (x:xs)=(div x 10):(mod x 10):xs