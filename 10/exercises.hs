import System.IO

-- Exercise 1
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x| x <- xs]

-- Exercise 2
type Board = [Int]

auxiliary :: Board -> Int -> IO ()
auxiliary [] _ = return ()
auxiliary (x:xs) n = do
    sequence_ (putStr (show n ++ ":") : [putStr " * " | _ <- [1..x]] ++ [putChar '\n'])
    auxiliary xs (n+1)

putBoard :: Board -> IO ()
putBoard b = auxiliary b 1

-- Exercise 3
putBoard' :: Board -> IO ()
putBoard' xs = sequence_ (concat [putStr (show n ++ ":") : [putStr " * " | _ <- [1..x]] ++ [putChar '\n'] | (x, n) <- zip xs [1..]])

-- Exercise 4
getNumber :: IO Int
getNumber = do
    x <- getLine
    return (read x)

accumulator :: Int -> Int -> IO Int
accumulator total 0 = return total
accumulator total n = do
    x <- getNumber
    accumulator (total + x) (n - 1)

adder :: IO ()
adder = do
    putStr "How many numbers? "
    n <- getNumber
    total <- accumulator 0 n
    putStrLn ("The total is " ++ (show total))

-- Exercise 5
adder' :: IO ()
adder' = do
    putStr "How many numbers? "
    n <- getNumber
    xs <- sequence [getNumber | _ <- [1..n]]
    total <- return (sum xs)
    putStrLn ("The total is " ++ (show total))

-- Exercise 6
getCh :: IO Char
getCh = do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x

backspace :: Char
backspace = '\DEL'

-- readLine with an accumulator
readLine' :: String -> IO String
readLine' xs = do
    x <- getCh
    if x == '\n' then
        return xs
    else if x == backspace then do
        if not (null xs) then do
            putStr "\b \b"
            readLine' (init xs)
        else
            readLine' xs
    else do
        putChar x
        readLine' (xs ++ [x])

readLine :: IO String
readLine = do
    readLine' ""

