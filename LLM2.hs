module LispLovesMe where

data AST = I32 Int
          |Sym String
          |Nul
          |Err
          |Lst [AST]
          |Boo Bool
          |Nod AST [AST]
         deriving (Eq, Show)
--

flst=[
      ("+",(\x->Just (I32 (sum (map extI32 x)))))
      ,("-",(\x->Just (I32 (foldl (-) ((extI32 . head) x ) (((map extI32) . (drop 1)) x)))))
      ,("*",(\x->Just (I32 (foldl (*) ((extI32 . head) x ) (((map extI32) . (drop 1)) x)))))
      ,("/",(\x->Just (I32 (foldl (div) ((extI32 . head) x ) (((map extI32) . (drop 1)) x)))))
      ,("list",\x->Just (Lst x))
      ,("^",\x->case x of
        ((I32 x):(I32 y):[])->Just (I32 (x^y))
        _->Nothing)
      ,(">",\x->case x of 
          ((I32 x):(I32 y):[])->Just (Boo (x>y))
          _->Nothing)
      ,(("<",\x->case x of 
          ((I32 x):(I32 y):[])->Just (Boo (x<y))
          _->Nothing))
      ,("!",(\x->case x of
        ((Boo x):[])->Just (Boo (not x))
        _->Nothing))
      ,("size",\x->case x of
        ((Lst x):[])->Just (I32 (length x))
        _->Nothing) 
      ,("reverse",\x->case x of
        ((Lst x):[])->Just (Lst (reverse x))
        _->Nothing) 
      ,("..",\x->case x of
        ((I32 x):(I32 y):[])->Just (Lst (map I32 [x..y]))
        _->Nothing)
      ,("==",\x->case x of 
          ((I32 x):(I32 y):[])->Just (Boo (x==y))
          _->Nothing)
      ,(">=",\x->case x of 
          ((I32 x):(I32 y):[])->Just (Boo (x>=y))
          _->Nothing)
      ,("<=",\x->case x of 
          ((I32 x):(I32 y):[])->Just (Boo (x<=y))
          _->Nothing)
      ,("!=",\x->case x of 
          ((I32 x):(I32 y):[])->Just (Boo (not (x==y)))
          _->Nothing)
      ,("if",\x->case x of
        ((Boo x):y:z:[])->Just (if x then y else z)
        _->Nothing)  
      ]

extI32 (I32 x)=x

spacLst=",\r\n\t "
numLst="0123456789"
charLst="_"++['a','b'..'z']++['A','B'..'Z']
opLst="~!@#$%^&*_+-=,./;'[]><"


lispEval :: String -> Maybe AST
lispEval s =let val=eval (('(':s)++")") [[]] in if val==Just Nul then Nothing else val


evalH ::[AST]->Maybe AST
evalH []=Just Nul
evalH (x:[])=Just x
evalH lt=(pick (last lt) flst) (drop 1 (reverse lt))


eval :: [Char]->[[AST]]->Maybe AST
eval [] (s:[])=evalH s 
eval _ []=Nothing

eval (x:xs) (s:stack)
  |elem x spacLst=eval xs (s:stack)
  |elem x numLst=do 
                (n,xs')<-getNum (x:xs) []
                eval xs' (((I32 (read n)):s):stack)
  |elem x (charLst++opLst)=do 
                (str,xs')<-getStr (x:xs) []
                eval xs' (((proc str):s):stack)
  |otherwise=case x of
                    '('->eval xs ([]:(s:stack))
                    ')'->do
                        val<-evalH s
                         
                        let stack'=(val:(head stack)):(drop 1 stack)
                        if  null xs then Just val else eval xs stack' 
                    _->Nothing

getNum :: String->String->Maybe (String,String)
getNum [] _=Nothing
getNum (x:xs) re
  |elem x numLst=getNum xs (x:re)
  |elem x (spacLst++")")=Just ((reverse re),x:xs)
  |otherwise=Nothing 

getStr :: String->String->Maybe (String,String)
getStr [] re=Nothing
getStr (x:xs) re
  |elem x (charLst++numLst++opLst)=getStr xs (x:re)
  |elem x (spacLst++")")=Just ((reverse re),x:xs)
  |otherwise=Nothing 

proc :: String->AST
proc x
  |x=="true"=Boo True
  |x=="false"=Boo False 
  |x=="null"=Nul
  |otherwise=Sym x

pick :: AST->[(String,([AST]->Maybe AST))]->([AST]->Maybe AST)
pick (Sym x) []=(\_->Nothing )
pick (Sym x) ((lab,f):fs)=if x==lab then f else pick (Sym x) fs
pick _ _=(\_->Nothing )


removeTwoSP (x:[])=if x==' ' then [] else [x]
removeTwoSP (x:y:xs)=if x==' '&& y==' ' then removeTwoSP (y:xs) else x:(removeTwoSP (y:xs))

removeFP (x:[])=[x]
removeFP (x:xs)=if not (x=='(')
                then x:(removeFP xs)
                else if (head xs)==' '
                        then removeFP (x:(drop 1 xs))
                        else  (x:(removeFP xs))


removeFP []=[]
removeSP (x:y:[])=if x==' ' then [y] else [x,y]
removeSP  (x:y:xs)=if and [y==')',x==' '] then removeSP (y:xs) else x:(removeSP (y:xs))
removeSP (x:[])=[x]

removeHead (x:xs)=if x==' ' then xs else x:xs



lispPretty :: String -> Maybe String
lispPretty s = Just ((removeSP . removeHead .removeFP . removeTwoSP) s)

