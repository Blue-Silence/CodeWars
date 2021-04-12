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

lastDigitH len (x:[])=(mod x (10^len))

lastDigitH len (x:0:[])=1
lastDigitH len (x:y:[])=let y'=mod y (10^(len+1)) in if y'==0 then lastDigitLen len x (10^(len+1)) else lastDigitLen len x y'
lastDigitH len (x:y:z:xs)
            |not (z==0)=let x'=(mod x (10^len))
                            y'=(mod y (10^(len+1)))
                            y''= correct (len+1) y y' 
                            xy'= lastDigitLen len x' y''
                        in if selfEqual len xy' then xy' else  lastDigitLen len x' (correct (len+1) y (lastDigitH (len+1) (y'':z:xs)))
            |otherwise=let  y'=(lastDigitH (len+1) (y:z:xs))
                            x'=(mod x (10^len))
                        in if x'==(mod (x'^2) (10^len)) then x' else (if y'==0 then  lastDigitLen len x' (10^(len+1)) else (lastDigitLen len x' y'))

correct len y y'
            |y==0=y'
            |y'==0=(10^len)
            |otherwise=y'

selfEqual len x 
            |(mod x (10^len))==mod ((mod x (10^len))^2) (10^len)=True 
            |otherwise=False 