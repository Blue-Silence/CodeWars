module EvaluateMathematicalExpression (calc) where

calc :: String -> Double
calc str= let (Num re)=(eval (Exp (negPro (parse str)))) in re




alpNum="0123456789."
alpOpt :: [Char]
alpOpt="+-*/"
data Exp=Minus Exp|Opt Char|Num Double|Exp [Exp] deriving Show
parse :: String->[Exp]
parse []=[]
parse (' ':xs)= (parse xs)
parse ('(':xs)=let (exp,rest)=readChunck 1 [] xs in exp : ( (parse rest))
parse (x:xs)
    |elem x alpNum =let (num,rest)=readNum [] (x:xs) in num : ( (parse rest))
    |elem x alpOpt =Opt x:( (parse xs)) 

readChunck 1 re (')':rest)=(Exp (parse (reverse re)),rest)
readChunck n re (')':rest)=readChunck (n-1) (')':re) rest 
readChunck n re ('(':rest)=readChunck (n+1) ('(':re) rest 
readChunck n re (x:xs)=readChunck n (x:re) xs 

readNum re []=((Num ((read . reverse) re)),[]) 
readNum re (x:xs)=if elem x alpNum then  readNum (x:re) xs else ((Num ((read . reverse) re)),x:xs)

negPro ((Opt '-'):x:xs)=(Minus x):(negPro' xs)
negPro x=negPro' x
negPro' []=[]
negPro' ((Opt a):(Opt '-'):x:y)=Opt a: (Minus x):(negPro' y)
negPro' ((Exp x):xs)=(Exp (negPro x)):(negPro' xs)
negPro' ((Minus (Exp x)):xs)=(Minus (Exp (negPro x))):(negPro' xs)
negPro' (x:xs)=x:negPro' xs  

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

eval (Num x)=Num x
eval (Minus x)=let (Num x')=eval x in (Num (-x'))
eval (Exp exp)=let (re:[])=(evalSnd . evalFst) exp in eval re 