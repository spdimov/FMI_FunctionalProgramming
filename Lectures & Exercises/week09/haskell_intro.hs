
--зад 1
-- Да се напише функцията fibonacci, която изчислава n-тото число на Фибоначи

fibonacci :: Int -> Int 
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- зад 2
-- Напишете горната фунцкия по повече начини

fibonacciIfElse n = if n < 2 then n else fibonacciIfElse (n-1) + fibonacciIfElse (n-2)

fibonacciGuards n
    | n < 2     = n
    | otherwise = fibonacciGuards (n-1) + fibonacciGuards (n-2)


--зад 3
-- . Да се напише функция fastPow, която степенува реално число x на неотрицателна степен n по метода на бързото степенуване

fastPow :: Int -> Int -> Int 
fastPow 0 _ = 0
fastPow _ 0 = 1
fastPow n pow
    | even pow = half * half
    |otherwise = half * half * n
    where half = fastPow n (pow `div` 2)

--зад 4
--Да се напишат функции complAdd, complSub и complMul, които извършват съответните операции над комплексни числа, представени като наредени двойки от координатите си

complAdd :: (Int,Int) -> (Int,Int) -> (Int,Int)
complAdd (x1,y1) (x2,y2) = (x1+x2,y1+y2)

complSub :: (Int,Int) -> (Int,Int) -> (Int,Int)
complSub (x1,y1) (x2,y2) = (x1-x2,y1-y2)

complMul :: (Int,Int) -> (Int,Int) -> (Int,Int)
complMul (x1,y1) (x2,y2) = (x1*x2,y1*y2)

--зад 5
--Да се напише функция distance, която намира разстоянието между две точки в равнината (наредени двойки)

--зад 6

-- Нека f е функция на един аргумент и n е цяло неотрицателно число. Дефинираме n-тото прилагане на функцията f да бъде функцията, дефинирана по следния начин:
-- f0(x) = x
-- fn(x) = f(fn-1(x))
-- Да се напише функция repeated, която връща n-тото прилагане на f.

repeated :: (Eq t, Num t) => (b -> b) -> t -> b -> b
repeated _ 0 = id
repeated f n = f.repeated f(n-1)

