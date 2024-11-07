# Задачи первой лаборатной на Haskell по предмету функциональное программирование

Решения выполнены с учетом ограничения: используются только функции, доступные в основном модуле `Prelude`, который подключается автоматически, если явно не указано обратное.

## Список задач
1. [Задача 2: Проверка, является ли список палиндромом](#задача-2-проверка-является-ли-список-палиндромом)
2. [Задача 30: Задача про покупку скота](#задача-30-задача-про-покупку-скота)
3. [Задача 32: Кодирование списка с подсчетом повторяющихся элементов](#задача-32-кодирование-списка-с-подсчетом-повторяющихся-элементов)
4. [Задача 45: Найти все трёхзначные простые числа](#задача-45-найти-все-трёхзначные-простые-числа)
5. [Задача 55: Сортировка списков по частоте длин](#задача-55-сортировка-списков-по-частоте-длин)
6. [Задача 2.13: Нахождение суммы ряда с точностью ε](#задача-213-нахождение-суммы-ряда-с-точностью-ε)

---

## Задача 2: Проверка, является ли список палиндромом

### Условие:
Написать функцию, которая проверяет, является ли заданный список палиндромом (последовательность, которая читается одинаково слева направо и справа налево).

### Решение:
```haskell
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs
```
Функция сравнивает список с его перевёрнутой версией. Если они равны, список является палиндромом.

## Задача 30: Задача про покупку скота

### Условие:
Имеется 100 рублей. Сколько быков, коров и телят можно купить на все эти деньги, если плата за быка – 10 рублей, за корову – 5 рублей, за теленка – 0.5 рубля и надо купить 100 голов скота?

### Решение:

```haskell
cattleProblem :: [(Int, Int, Int)]
cattleProblem = [(bulls, cows, calves) | 
    bulls <- [0..10], cows <- [0..20], calves <- [0..200], 
    bulls + cows + calves == 100, 
    100 * bulls + 50 * cows + 5 * calves == 1000]
```
Функция возвращает список возможных комбинаций покупки быков, коров и телят, при которых общее количество голов скота и общая стоимость составляют 100.

## Задача 32: Кодирование списка с подсчетом повторяющихся элементов

### Условие:
Дан список из n элементов. Разбить список на подсписки одинаковых элементов идущих подряд и подсчитать их количество: “aaaabccaadeeee” -> [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

### Решение:
```haskell
groupConsecutive :: Eq a => [a] -> [(Int, a)]
groupConsecutive [] = []
groupConsecutive (x:xs) = 
  let (first, rest) = span (== x) xs
      count = 1 + length first
  in (count, x) : groupConsecutive rest
```
Функция возвращает список пар, где каждый элемент содержит количество подряд идущих одинаковых элементов и сам элемент.

## Задача 45: Найти все трёхзначные простые числа

### Условие:
Написать функцию, которая возвращает список всех трёхзначных простых чисел.

### Решение:
```haskell
isPrime :: Int -> Bool
isPrime n
  | n <= 1    = False
  | n == 2    = True
  | even n    = False
  | otherwise = not (any divides [3,5..limit])
  where
    limit = floor (sqrt (fromIntegral n))
    divides x = n `mod` x == 0

threeDigitPrimes :: [Int]
threeDigitPrimes = filter isPrime [100..999]
```
Функция возвращает список всех простых чисел в диапазоне от 100 до 999.

## Задача 55: Сортировка списков по частоте длин

### Условие:
Дан список из n списков. Необходимо отсортировать его по частотам длин списков: lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"] -> ["ijkl","o","abc","fgh","de","de","mn"]
То есть списки с одинаковой длиной должны быть сгруппированы, а затем отсортированы по частоте их длины.

### Решение:
```haskell
-- Функция для подсчета частоты каждой длины
lengthFrequency :: [String] -> [(Int, Int)]
lengthFrequency xs = countLengths uniqueLengths
  where
    lengths = map length xs
    uniqueLengths = getUnique lengths

    getUnique [] = []
    getUnique (y:ys) = y : getUnique (filter (/= y) ys)

    countLengths [] = []
    countLengths (l:ls) = (l, length (filter (== l) lengths)) : countLengths ls

-- Функция для сортировки строк по частоте длины
sortStringsByLengthFrequency :: [String] -> [String]
sortStringsByLengthFrequency xs = reverse (go sortedFrequency [])
  where
    frequency = lengthFrequency xs
    sortedFrequency = sortFrequency frequency

    -- Сортируем по возрастанию частоты
    sortFrequency [] = []
    sortFrequency (x:xs) = sortFrequency [(l, f) | (l, f) <- xs, f >= snd x] ++ 
                           [x] ++ 
                           sortFrequency [(l, f) | (l, f) <- xs, f < snd x]

    go [] acc = acc
    go ((len, _):ys) acc = go ys (acc ++ filter ((== len) . length) xs)
```

## Вся программа:
```haskell
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

cattleProblem :: [(Int, Int, Int)]
cattleProblem = [(bulls, cows, calves) | 
    bulls <- [0..10], cows <- [0..20], calves <- [0..200], 
    bulls + cows + calves == 100, 
    100 * bulls + 50 * cows + 5 * calves == 1000]
  
groupConsecutive :: Eq a => [a] -> [(Int, a)]
groupConsecutive [] = []
groupConsecutive (x:xs) = 
  let (first, rest) = span (== x) xs
      count = 1 + length first
  in (count, x) : groupConsecutive rest

isPrime :: Int -> Bool
isPrime n
  | n <= 1    = False
  | n == 2    = True
  | even n    = False
  | otherwise = not (any divides [3,5..limit])
  where
    limit = floor (sqrt (fromIntegral n))
    divides x = n `mod` x == 0

threeDigitPrimes :: [Int]
threeDigitPrimes = filter isPrime [100..999]

-- Функция для подсчета частоты каждой длины
lengthFrequency :: [String] -> [(Int, Int)]
lengthFrequency xs = countLengths uniqueLengths
  where
    lengths = map length xs
    uniqueLengths = getUnique lengths

    getUnique [] = []
    getUnique (y:ys) = y : getUnique (filter (/= y) ys)

    countLengths [] = []
    countLengths (l:ls) = (l, length (filter (== l) lengths)) : countLengths ls

-- Функция для сортировки строк по частоте длины
sortStringsByLengthFrequency :: [String] -> [String]
sortStringsByLengthFrequency xs = reverse (go sortedFrequency [])
  where
    frequency = lengthFrequency xs
    sortedFrequency = sortFrequency frequency

    -- Сортируем по возрастанию частоты
    sortFrequency [] = []
    sortFrequency (x:xs) = sortFrequency [(l, f) | (l, f) <- xs, f >= snd x] ++ 
                           [x] ++ 
                           sortFrequency [(l, f) | (l, f) <- xs, f < snd x]

    go [] acc = acc
    go ((len, _):ys) acc = go ys (acc ++ filter ((== len) . length) xs)
    
main :: IO ()
main = do
  putStrLn "Testing task 1: isPalindrome"  
  let test1 = isPalindrome "abcba"
  putStrLn $ "abcba test: " ++ show test1  
  let test2 = isPalindrome "abc"
  putStrLn $ "abc test: " ++ show test2  
  let test3 = isPalindrome ""
  putStrLn $ "'' test: " ++ show test3  
  let test4 = isPalindrome [1, 2, 3, 2, 1]
  putStrLn $ "[1, 2, 3, 2, 1] test: " ++ show test4  
  let test5 = isPalindrome [1, 2, 3, 4]
  putStrLn $ "[1, 2, 3, 4] test: " ++ show test5
  putStrLn "---"
  putStrLn "Testing task 2: cattleProblem"  
  print cattleProblem
  putStrLn "---"
  putStrLn "Testing task 3: groupConsecutive. Test data: aaaabccaadeeee"  
  let input = "aaaabccaadeeee"
  let result = groupConsecutive input  
  print result
  putStrLn "---"
  putStrLn "Testing task 4: all three-digit prime numbers." 
  print threeDigitPrimes
  putStrLn "---"
  putStrLn "Testing task 5: sort by length frequency. Test data: [abc, de, fgh, de, ijkl, mn, o] " 
  print $ sortStringsByLengthFrequency ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
  putStrLn "---"
  
```

## Задача 2.13: Нахождение суммы ряда с точностью ε
## Явная рекурсия
```haskell
sumSeries :: Double -> Double -> Double -> Int -> Double
sumSeries x epsilon currentSum n
    | abs term < epsilon = currentSum  -- Если модуль текущего слагаемого меньше epsilon, возвращаем сумму
    | otherwise = sumSeries x epsilon newSum (n + 1)  -- Иначе продолжаем рекурсию
  where
    term = calculateTerm x n  -- Вычисляем текущее слагаемое
    newSum = currentSum + term  -- Обновляем сумму

calculateTerm :: Double -> Int -> Double
calculateTerm x n = sign * (fromIntegral numerator / fromIntegral denominator) * (x ** fromIntegral n)
  where
    sign = if even n then 1 else -1  -- Чередуем знак
    numerator = product [1 + 2 * i | i <- [0..(n-1)]]  -- числ 1 * 3 * 5 * ... (2n-1)
    denominator = product [4 + 2 * i | i <- [0..(n-1)]]  -- знам 4 * 6 * 8 * ... (2n+2)

calculateSum :: Double -> Double -> Double
calculateSum x epsilon = sumSeries x epsilon 0 0  -- Начинаем с суммы 0 и индекса 0

main :: IO ()
main = do
    let x = 1.0
    let epsilon = 0.07
    let result = calculateSum x epsilon
    putStrLn $ "Сумма ряда: " ++ show result
    putStrLn $ "Контрольная формула: " ++ show (2 * sqrt (1 + x) - 2)

```
##  С использованием бесконечных списков и функций zip, map или zipWith без явного использования рекурсии
```haskell
term :: Int -> Double -> Double
term n x = sign * (fromIntegral num / fromIntegral denom) * (x ** fromIntegral n)
  where
    sign = if even n then 1 else -1
    num = product [1 + 2 * i | i <- [0..(n-1)]]
    denom = product [4 + 2 * i | i <- [0..(n-1)]]

series :: Double -> [Double]
series x = map (\n -> term n x) [0..]

sumSeries :: Double -> Double -> Double
sumSeries x epsilon = sum $ takeWhile (\t -> abs t >= epsilon) (series x)

main :: IO ()
main = do
    let x = 1.0 
    let epsilon = 0.07
    let result = sumSeries x epsilon
    putStrLn $ "Сумма ряда: " ++ show result
    putStrLn $ "Контрольная формула: " ++ show (2 * sqrt (1 + x) - 2)

```
