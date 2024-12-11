# Задачи первой лаборатной на Haskell по предмету функциональное программирование

Решения выполнены с учетом ограничения: используются только функции, доступные в основном модуле `Prelude`, который подключается автоматически, если явно не указано обратное.

## Список задач первой лабораторной
1. [Задача 2: Проверка, является ли список палиндромом](#задача-2-проверка-является-ли-список-палиндромом)
2. [Задача 30: Задача про покупку скота](#задача-30-задача-про-покупку-скота)
3. [Задача 32: Кодирование списка с подсчетом повторяющихся элементов](#задача-32-кодирование-списка-с-подсчетом-повторяющихся-элементов)
4. [Задача 45: Найти все трёхзначные простые числа](#задача-45-найти-все-трёхзначные-простые-числа)
5. [Задача 55: Сортировка списков по частоте длин](#задача-55-сортировка-списков-по-частоте-длин)
6. [Задача 2.13: Нахождение суммы ряда с точностью ε](#задача-213-нахождение-суммы-ряда-с-точностью-ε)

## Список задач второй лабораторной
1. [Задача 1: Придумать свой класс типов](#задача-1-придумать-свой-класс-типов)
2. [Задача 7: Поиск в бинарном дереве](#задача-7-поиск-в-бинарном-дереве)
3. [Задача 17: Каталог книг](#задача-17-каталог-книг)

## Список задач третьей лабораторной
1. [Задача 1: Апгрейд задачи 17 из второй лабораторной](#задача-1-Апгрейд-задачи-17-из-второй-лабораторной)
2. [Задача 3: Сдача сессии студентом](#задача-1-Сдача-сессии-студентом)

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
calculateTerm x n = sign * (fromIntegral numerator / fromIntegral denominator) * (x ** fromIntegral (n+1))
  where
    sign = if even n then 1 else -1  -- Чередуем знак
    numerator = product [1 + 2 * i | i <- [0..(n-1)]]  -- числ 1 * 3 * 5 * ... (2n-1)
    denominator = product [4 + 2 * i | i <- [0..(n-1)]]  -- знам 4 * 6 * 8 * ... (2n+2)

calculateSum :: Double -> Double -> Double
calculateSum x epsilon = sumSeries x epsilon 0 0  -- Начинаем с суммы 0 и индекса 0

main :: IO ()
main = do
    let x = 0.5
    let epsilon = 0.0001
    let result = calculateSum x epsilon
    let check = (2 * sqrt (1 + x) - 2)
    putStrLn $ "Сумма ряда: " ++ show result
    putStrLn $ "Контрольная формула: " ++ show check
    putStrLn $ "Разница: " ++ show (check - result)
```
##  С использованием бесконечных списков и функций zip, map или zipWith без явного использования рекурсии
```haskell
term :: Int -> Double -> Double
term n x = sign * (fromIntegral num / fromIntegral denom) * (x ** fromIntegral (n+1))
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
    let x = 0.5
    let epsilon = 0.0001
    let result = sumSeries x epsilon
    let check = (2 * sqrt (1 + x) - 2)
    putStrLn $ "Сумма ряда: " ++ show result
    putStrLn $ "Контрольная формула: " ++ show check
    putStrLn $ "Разница: " ++ show (check - result)
```

## Задача 1: Придумать свой класс типов
Придумать свой класс типов, который содержит как минимум
две функции, одна из которых выражается через другие.
Написать реализацию этого класса типов для любых двух типов
данных, типы данных выбирать такие, чтобы их реализации
отличались (можно использовать свои собственные типы
данных).

```haskell
class Calculable a where
    calc1 :: a -> a -> a
    calc2 :: a -> a -> a
    calc2 x y = calc1 x y `calc1` y

instance Calculable Int where
    calc1 x y = x + y

data Point = Point Int Int deriving Show

instance Calculable Point where
    calc1 (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

main :: IO ()
main = do
    let intResult1 = calc1 (3 :: Int) (5 :: Int)
    let intResult2 = calc2 (3 :: Int) (5 :: Int)
    putStrLn $ "calc1 for Int: " ++ show intResult1
    putStrLn $ "calc2 for Int: " ++ show intResult2
    
    let p1 = Point 1 2
    let p2 = Point 3 4
    let pointResult1 = calc1 p1 p2
    let pointResult2 = calc2 p1 p2
    putStrLn $ "calc1 for Point: " ++ show pointResult1
    putStrLn $ "calc2 for Point: " ++ show pointResult2
```

## Задача 7: Поиск в бинарном дереве
Дано бинарное дерево, определить содержит ли оно элемент E. 
```haskell
data MyBinaryTree a = Empty | Node a (MyBinaryTree a) (MyBinaryTree a) deriving Show

data MyBool = MyTrue | MyFalse deriving Show

contains :: Eq a => a -> MyBinaryTree a -> MyBool
contains _ Empty = MyFalse
contains e (Node x left right)
    | x == e  = MyTrue
    | otherwise   = case contains e left of
                      MyTrue -> MyTrue
                      MyFalse -> contains e right

main :: IO ()
main = do
    let tree = Node 5
                (Node 3
                    (Node 2 Empty Empty)
                    (Node 4 Empty Empty))
                (Node 7
                    (Node 6 Empty Empty)
                    (Node 8 Empty Empty))
    let e = 2
    putStrLn $ "Does the tree contain " ++ show e ++ "? " ++ show (contains e tree)
```

## Задача 17: Каталог книг
Создать тип данных каталог книг. Написать функции
добавления книг в каталог, удаления книг из каталога, поиска
книг в каталоге по разным критериям (например по автору,
названию, жанру и т.д.). Написать функцию сортировки книг в
каталоге, поддержать возможность сортировать по разным
критериям.

```haskell
import Data.List (sortOn)

type Author = String
type Title = String
type Genre = String

data Book = Book {
    title :: Title,
    author :: Author,
    genre :: Genre
} deriving (Show, Eq)

data Catalog = Catalog [Book] deriving Show

data SortCriterion = SortByTitle | SortByAuthor | SortByGenre deriving (Show, Eq)

data SearchCriterion = SearchByTitle Title | SearchByAuthor Author | SearchByGenre Genre deriving (Show, Eq)

addBook :: Book -> Catalog -> Catalog
addBook book (Catalog books) = Catalog (book : books)

removeBook :: Book -> Catalog -> Catalog
removeBook book (Catalog books) = Catalog (filter (/= book) books)

searchBooks :: SearchCriterion -> Catalog -> [Book]
searchBooks (SearchByTitle titleName) (Catalog books) = filter (\b -> title b == titleName) books
searchBooks (SearchByAuthor authorName) (Catalog books) = filter (\b -> author b == authorName) books
searchBooks (SearchByGenre genreName) (Catalog books) = filter (\b -> genre b == genreName) books

sortCatalog :: SortCriterion -> Catalog -> [Book]
sortCatalog SortByTitle (Catalog books) = sortOn title books
sortCatalog SortByAuthor (Catalog books) = sortOn author books
sortCatalog SortByGenre (Catalog books) = sortOn genre books

main :: IO ()
main = do
    let book1 = Book "Book B" "Author C" "Fantasy"
    let book2 = Book "Book A" "Author D" "Science Fiction"
    let book3 = Book "Book D" "Author A" "Drama"
    let book4 = Book "Book C" "Author B" "Detective"
    let catalog = addBook book1 $ addBook book2 $ addBook book3 $ addBook book4 $ Catalog []

    putStrLn "Catalog after adding books:"
    print catalog

    let catalogAfterRemoval = removeBook book2 catalog
    putStrLn "\nCatalog after removing book 2:"
    print catalogAfterRemoval

    putStrLn "\nSearch by author 'Author A':"
    print $ searchBooks (SearchByAuthor "Author A") catalog

    putStrLn "\nSearch by title 'Book A':"
    print $ searchBooks (SearchByTitle "Book A") catalog

    putStrLn "\nSearch by genre 'Fantasy':"
    print $ searchBooks (SearchByGenre "Fantasy") catalog

    putStrLn "\nCatalog sorted by title:"
    print $ sortCatalog SortByTitle catalog

    putStrLn "\nCatalog sorted by author:"
    print $ sortCatalog SortByAuthor catalog

    putStrLn "\nCatalog sorted by genre:"
    print $ sortCatalog SortByGenre catalog

```

## Задача 1: Апгрейд задачи 17 из второй лабораторной
Для задачи 17 из второй лабораторной реализован консольный интерфейс, используя монаду IO и do нотацию.
Интерфейс дает возможность выполнить все
реализованные функции. Программа завершается только
после того как пользователь выберет ответствующую опцию, то
есть можно выполнить несколько функций
до завершения программы.
```haskell
import Data.List (sortOn)
import System.IO (hFlush, stdout)

type Author = String
type Title = String
type Genre = String

data Book = Book {
    title :: Title,
    author :: Author,
    genre :: Genre
} deriving (Show, Eq)

data Catalog = Catalog [Book] deriving Show

data SortCriterion = SortByTitle | SortByAuthor | SortByGenre deriving (Show, Eq)

data SearchCriterion = SearchByTitle Title | SearchByAuthor Author | SearchByGenre Genre deriving (Show, Eq)

addBook :: Book -> Catalog -> Catalog
addBook book (Catalog books) = Catalog (book : books)

removeBook :: Book -> Catalog -> Catalog
removeBook book (Catalog books) = Catalog (filter (/= book) books)

searchBooks :: SearchCriterion -> Catalog -> [Book]
searchBooks (SearchByTitle titleName) (Catalog books) = filter (\b -> title b == titleName) books
searchBooks (SearchByAuthor authorName) (Catalog books) = filter (\b -> author b == authorName) books
searchBooks (SearchByGenre genreName) (Catalog books) = filter (\b -> genre b == genreName) books

sortCatalog :: SortCriterion -> Catalog -> [Book]
sortCatalog SortByTitle (Catalog books) = sortOn title books
sortCatalog SortByAuthor (Catalog books) = sortOn author books
sortCatalog SortByGenre (Catalog books) = sortOn genre books

showMenu :: IO ()
showMenu = do
    putStrLn "Choose an option:"
    putStrLn "1. Add a book"
    putStrLn "2. Remove a book"
    putStrLn "3. Search for books"
    putStrLn "4. Sort the catalog"
    putStrLn "5. Show the catalog"
    putStrLn "6. Exit"
    putStr "Enter your choice: "
    hFlush stdout

addBookFromConsole :: Catalog -> IO Catalog
addBookFromConsole catalog = do
    putStrLn "Enter the title of the book:"
    title <- getLine
    putStrLn "Enter the author of the book:"
    author <- getLine
    putStrLn "Enter the genre of the book:"
    genre <- getLine
    let book = Book title author genre
    return (addBook book catalog)

removeBookFromConsole :: Catalog -> IO Catalog
removeBookFromConsole catalog = do
    putStrLn "Enter the title of the book to remove:"
    title <- getLine
    putStrLn "Enter the author of the book to remove:"
    author <- getLine
    putStrLn "Enter the genre of the book to remove:"
    genre <- getLine
    let book = Book title author genre
    return (removeBook book catalog)

searchBooksFromConsole :: Catalog -> IO ()
searchBooksFromConsole catalog = do
    putStrLn "Choose a search criterion:"
    putStrLn "1. By title"
    putStrLn "2. By author"
    putStrLn "3. By genre"
    putStr "Enter your choice: "
    hFlush stdout
    criterionChoice <- getLine
    case criterionChoice of
        "1" -> do
            putStrLn "Enter the title to search for:"
            title <- getLine
            let results = searchBooks (SearchByTitle title) catalog
            print results
        "2" -> do
            putStrLn "Enter the author to search for:"
            author <- getLine
            let results = searchBooks (SearchByAuthor author) catalog
            print results
        "3" -> do
            putStrLn "Enter the genre to search for:"
            genre <- getLine
            let results = searchBooks (SearchByGenre genre) catalog
            print results
        _ -> putStrLn "Invalid choice"

sortCatalogFromConsole :: Catalog -> IO ()
sortCatalogFromConsole catalog = do
    putStrLn "Choose a sort criterion:"
    putStrLn "1. By title"
    putStrLn "2. By author"
    putStrLn "3. By genre"
    putStr "Enter your choice: "
    hFlush stdout
    criterionChoice <- getLine
    case criterionChoice of
        "1" -> do
            let sortedCatalog = sortCatalog SortByTitle catalog
            print sortedCatalog
        "2" -> do
            let sortedCatalog = sortCatalog SortByAuthor catalog
            print sortedCatalog
        "3" -> do
            let sortedCatalog = sortCatalog SortByGenre catalog
            print sortedCatalog
        _ -> putStrLn "Invalid choice"

main :: IO ()
main = do
    let catalog = Catalog []
    loop catalog

loop :: Catalog -> IO ()
loop catalog = do
    showMenu
    choice <- getLine
    case choice of
        "1" -> do
            newCatalog <- addBookFromConsole catalog
            loop newCatalog
        "2" -> do
            newCatalog <- removeBookFromConsole catalog
            loop newCatalog
        "3" -> do
            searchBooksFromConsole catalog
            loop catalog
        "4" -> do
            sortCatalogFromConsole catalog
            loop catalog
        "5" -> do
            print catalog
            loop catalog
        "6" -> putStrLn "Exiting..."
        _ -> do
            putStrLn "Invalid choice"
            loop catalog
```

## Задача 3: Сдача сессии студентом
С помощью монады Writer и do нотации написать функцию,
которая будет имитировать сдачу сессии студентом, то есть
монада Writer должна уметь сохранять в себе список предметов в
порядке их сдачи и сумму всех баллов за экзамены. Реализовать
функции, которые будут из монады Writer доставать список
сданных предметов в порядке их сдачи, и средний балл за все
экзамены.

```haskell
import Control.Monad.Writer

type SessionLog = ([String], Sum Int)

passExam :: String -> Int -> Writer SessionLog ()
passExam subject score = tell ([subject], Sum score)

session :: Writer SessionLog ()
session = do
    passExam "Math" 90
    passExam "Physics" 85
    passExam "History" 75
    passExam "Literature" 80

getObjects :: Writer SessionLog () -> [String]
getObjects w = let ((), (subjects, _)) = runWriter w in subjects

getAverageScore :: Writer SessionLog () -> Int
getAverageScore w =
    let ((), (subjects, Sum totalScore)) = runWriter w
        count = length subjects
    in if count > 0 then totalScore `div` count else 0

main :: IO ()
main = do
    let log = session
    putStrLn $ "Subjects: " ++ show (getObjects log)
    putStrLn $ "Average score: " ++ show (getAverageScore log)
```
