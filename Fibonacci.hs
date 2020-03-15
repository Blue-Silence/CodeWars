module Fibonacci where

lt=0:1:(zipWith (+) (lt) (drop 1 lt))
fib :: Integer -> Integer
fib x=if x<0 then (-((lt)!!(fromInteger (-x)))) else (lt!!(fromInteger x))