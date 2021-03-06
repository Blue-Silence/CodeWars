module LastDigit (lastDigit) where

lastDigit :: [Integer] -> Integer
lastDigit []=1
lastDigit as = mod (evalStep ((length as)-1) (reverse as)) 10

lastDigitLen :: Int->Integer -> Integer -> Integer
lastDigitLen len a b 
    |b>2=let n=div b 2 in if even b then (lastDigitLen len (lastDigitLen len a n) 2) else (lastNum len ((lastDigitLen len (lastDigitLen len a n) 2) * a ))
    |b==2=lastNum len ((lastNum len a)*(lastNum len a))
    |b==1=(lastNum len a)
    |b==0=1    
lastNum len x=(mod x (10^len))

evalStep len (x:[])=x
evalStep len (0:0:xs)=evalStep (len-1) (1:xs)
evalStep len (x:y:xs)=let n=lastDigitLen len y x in if n==0 then evalStep (len-1) (10^len : xs) else evalStep (len-1) (n : xs)