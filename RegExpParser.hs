module RegExpParser
       ( RegExp(..)
       , parseRegExp
       ) where

data RegExp = Normal Char       -- ^ A character that is not in "()*|."
            | Any               -- ^ Any charater
            | ZeroOrMore RegExp -- ^ Zero or more occurances of the same regexp
            | Or RegExp RegExp  -- ^ A choice between 2 regexps
            | Str [RegExp]      -- ^ A sequence of regexps.
  deriving (Show, Eq)

parseRegExp :: String -> Maybe RegExp
parseRegExp s = parser (s,[])

specList="()*|."
specList'="()*|" --remove '.'
specList''=")*|"

getStr :: (String,[RegExp])->(String,[RegExp])
getStr ([],re)=([],re)
getStr (x:xs,re)=if x=='.' then getStr (xs,Any:re) else if elem x specList then (x:xs,re) else getStr (xs,(Normal x):re)

parser :: (String,[RegExp])-> Maybe RegExp
parser ([],(re:[]))=Just re
parser ([],re)=if null re then Nothing else Just (Str (reverse re)) 
parser (x:xs,stack)
    |not (elem x specList')=let (xs',s')=getStr ((x:xs),[]) in parser (xs',s'++stack) 
    |otherwise=case x of
                '('->do 
                    (inP,afterP)<-paF 1 [] xs
                    s'<-parser (inP,[])
                    parser ((afterP,s':stack))
                ')'->Nothing
                '*'->do 
                    stack'<-fStar stack
                    parser (xs,stack')
                '|'-> do 
                    s'<-parseRegExp xs
                    stack'<-fOr stack s' 
                    parser ([],stack')  

fStar []=Nothing
fStar (a:as)=Just ((ZeroOrMore a):as)       

paF :: Int->[Char]->[Char]->Maybe ([Char],[Char])
paF 0 re xs=Just ((reverse re),xs)
paF _ _ []=Nothing
paF n re (x:xs)=case x of 
                '('->paF (n+1) re xs 
                ')'->paF (n-1) re xs 
                _->paF n (x:re) xs

fOr [] _=Nothing
fOr (x:[]) s=Just ((Or x s):[])
fOr xs s=Just ((Or (Str (reverse xs)) s):[])