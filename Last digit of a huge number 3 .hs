module LastDigit (lastDigit) where

lastDigit :: [Integer] -> Integer
lastDigit []=1
lastDigit xs=lastDigitH 1 xs

lastDigitLen :: Int->Integer -> Integer -> Integer
lastDigitLen len a b 
    |b>2=let n=div b 2 in if even b then (lastDigitLen len (lastDigitLen len a n) 2) else (lastNum len ((lastDigitLen len (lastDigitLen len a n) 2) * a ))
    |b==2=lastNum len ((lastNum len a)*(lastNum len a))
    |b==1=(lastNum len a)
    |b==0=1    
lastNum len x=(mod x (10^len))

lastDigitH len (0:0:[])=1
lastDigitH len (x:[])=(mod x (10^len))
lastDigitH len (x:y:xs) 
        |not (y==0)=let y'=(lastDigitH (len+1) (y:xs))
                        x'=(mod x (10^len))
                            in (if x'==(mod (x'^2) (10^len)) then undefined {-x'-} else (if y'==0 then  lastDigitLen len x' (10^(len+1)) else (lastDigitLen len x' y')))
        |otherwise=let x'=(mod x (10^len)) in if x'==(mod (x^2) (10^len)) then x' else (lastDigitLen len x' (lastDigitH (len+1) (y:xs)))