-- run cabal install --lib random to have randomRIO
import System.Random (randomRIO)
import Data.Char
import Data.List
import System.IO

-- used to be called depth
maxdepth :: Int
maxdepth = 9

size :: Int
size = 3

type Grid = [[Player]]

data Player = O | B | X
              deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
         where
            os = length (filter (== O) ps)
            xs = length (filter (== X) ps)
            ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
              line = all (== p)
              rows = g
              cols = transpose g
              dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid =
   putStrLn . unlines . concat . interleave bar . map showRow
   where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where
             beside = foldr1 (zipWith (++))
             bar    = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move:: Grid -> Int -> Player -> [Grid]
move g i p =
   if valid g i then [chop size (xs ++ [p] ++ ys)] else []
   where (xs,B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                      return (read xs)
                   else
                      do putStrLn "ERROR: Invalid number"
                         getNat prompt

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int,Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

data Tree a = Node a [Tree a]
              deriving Show

-- Board + depth
type GameState = (Grid, Int)

gametree :: GameState -> Player -> Tree GameState
gametree (g, depth) p = Node (g, depth) [gametree (g', depth + 1) (next p) | g' <- moves g p, depth < maxdepth]

moves :: Grid -> Player -> [Grid]
moves g p | won g     = []
          | full g    = []
          | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]


-- Board + depth + player
type PlayerGameState = (Grid, Int, Player)

minimax :: Tree GameState -> Tree PlayerGameState
minimax (Node (g,d) [])
   | wins O g  = Node (g,d,O) []
   | wins X g  = Node (g,d,X) []
   | otherwise = Node (g,d,B) []
minimax (Node (g,d) ts)
   | turn g == O = Node (g, d, minimum ps) ts'
   | turn g == X = Node (g, d, maximum ps) ts'
                   where
                      ts' = map minimax ts
                      ps  = [p | Node (_,_,p) _ <- ts']

randomElement :: [a] -> IO a
randomElement xs = do
    index <- randomRIO (0, (length xs - 1))
    return (xs !! index)

bestmoves :: Grid -> Player -> [GameState]
bestmoves g p = [(g', depth) | Node (g',depth,p') _ <- ts, p' == best]
               where
                  tree = gametree (g, 0) p
                  Node (_,_,best) ts = minimax tree

bestmove :: [GameState] -> GameState
bestmove [x] = x
bestmove ((g,d):xs) = if d < snd (bestmove xs) then (g,d) else bestmove xs

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    human <- choosePlayer
    putStrLn "computing game tree. Please wait ..."
    tree <- return $! minimax (gametree (empty, 0) (next human))
    --tree <- return $! alphaBeta (gametree (empty, 0) (next human)) human O X
    putStrLn "Done!"
    play tree human empty O

choosePlayer :: IO Player
choosePlayer = do
    putStrLn "Type O if you want to go first, type X if you want to go second"
    x <- getChar
    if x == 'X' then
        return X
    else if x == 'O' then
        return O
    else do
        putStrLn "\nInvalid input"
        choosePlayer

play :: Tree PlayerGameState -> Player -> Grid -> Player -> IO ()
play t human g p = do
    cls
    goto (1,1)
    putGrid g
    play' t human g p

findChild :: Grid -> Tree PlayerGameState -> Tree PlayerGameState
findChild g (Node n ((Node m ts) :ts')) = if g == g' then Node m ts else findChild g (Node n ts')
    where
        (g',_,_) = m

play' :: Tree PlayerGameState -> Player -> Grid -> Player -> IO ()
play' t human g p
    | wins O g      = putStrLn "Player O wins!\n"
    | wins X g      = putStrLn "Player X wins!\n"
    | full g        = putStrLn "It's a draw!\n"
    | p == human    = do
        i <- getNat (prompt p)
        case move g i p of
            []   -> do
                putStrLn "ERROR: Invalid move"
                play' t human g p
            [g'] -> do
                t' <- return (findChild g' t)
                play t' human g' (next p)
    | p /= human    = do
        putStrLn "AI is thinking... "
        (g',_) <- return (bestmove (bestmoves g p))
        t' <- return (findChild g' t)
        play t' human g' (next p)

