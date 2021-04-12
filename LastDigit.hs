module LastDigit (lastDigit) where

lastDigit :: [Integer] -> Integer
lastDigit as = (lastDigit' . reverse ) as


lastCal _ 0=1
lastCal 0 _=1
lastCal x 1=x
lastCal x pow=let x'=lastCal (mod (x^2) 10) (div pow 2) in if even pow then x' else mod (x' * x) 10

lastDigit' []=1
lastDigit' (x:[])=x
lastDigit' (x:y:xs)=lastDigit' (lastCal y x:xs)


----------UNSOLVED!!!