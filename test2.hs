module EvaluateMathematicalExpression (calc) where

calc :: String -> Double
calc str= let (Num re)=(eval (Exp ( (parse str)))) in re


space="\n\t "

alpNum="0123456789."
alpOpt :: [Char]
alpOpt="+-*/"
data Exp=Minus|Opt Char|Num Double|Exp [Exp] deriving Show
parse :: String->[Exp]
parse []=[]
parse ('(':xs)=let (exp,rest)=readChunck 1 [] xs in exp : ( (parse rest))
parse (x:xs)
    |elem x space=parse xs
    |elem x alpNum =let (num,rest)=readNum [] (x:xs) in num : ( (parse rest))
    |x=='-' =let  (x':xs')=xs in  if elem x' space then Opt x:( (parse xs')) else Minus:(parse (x':xs'))
    |elem x alpOpt=Opt x:( (parse xs))

readChunck 1 re (')':rest)=(Exp (parse (reverse re)),rest)
readChunck n re (')':rest)=readChunck (n-1) (')':re) rest 
readChunck n re ('(':rest)=readChunck (n+1) ('(':re) rest 
readChunck n re (x:xs)=readChunck n (x:re) xs 

readNum re []=((Num ((read . reverse) re)),[]) 
readNum re (x:xs)=if elem x alpNum then  readNum (x:re) xs else ((Num ((read . reverse) re)),x:xs)

evalFst (x:(Opt '*'):y:ys)=let (Num xNum)=eval x
                               (Num yNum)=eval y
                           in evalFst ((Num (xNum*yNum)):ys)
evalFst (x:(Opt '/'):y:ys)=let (Num xNum)=eval x
                               (Num yNum)=eval y
                           in evalFst ((Num (xNum/yNum)):ys)
evalFst (x:xs)=x:(evalFst xs)                           
evalFst []=[] 


evalSnd (x:(Opt '+'):y:ys)=let (Num xNum)=eval x
                               (Num yNum)=eval y
                           in evalSnd ((Num (xNum+yNum)):ys)
evalSnd (x:(Opt '-'):y:ys)=let (Num xNum)=eval x
                               (Num yNum)=eval y
                           in evalSnd ((Num (xNum - yNum)):ys)
evalSnd (x:xs)=x:(evalSnd xs)
evalSnd []=[] 
evalMinus (Minus:Minus:xs)=evalMinus xs
evalMinus (Minus:x:xs)=let (Num num)=eval x in (Num (-num)):(evalMinus xs)

evalMinus (x:xs)=x:(evalMinus xs)
evalMinus []=[]

eval (Num x)=Num x
eval (Exp exp)=let (re:[])=(merge . evalSnd . evalFst . evalMinus) exp in eval re 

merge (Num x:[])=(Num x):[]
merge ((Num x):(Num y):xs)=merge ((Num( x+y)):xs)
merge x=x