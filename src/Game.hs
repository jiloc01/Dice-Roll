module Game(
  startGame
)where

import System.Random
 
data Player = Player { score :: Int, name :: String }
mkPlayer :: String -> Player
mkPlayer name = Player { score = 0, name = name }

game :: [Int] -> Int -> Player -> Player -> Player
game rands threshold p1 p2
     | score p1 >= threshold = p1
     | score p2 >= threshold = p2
     | otherwise             = game (tail rands) threshold p2 p1 { score = score p1 + head rands }
 
diceRoll :: Int -> Int
diceRoll x = 1 + x `mod` 6



player_name :: Int -> IO Player
player_name n = do
    putStrLn $ "Введите имя игрока №" ++ show n 
    pname <- getLine
    return (mkPlayer  pname)
    
threshold_value :: IO Int
threshold_value = do
    putStrLn $ "Введите пороговое значение"
    value <- getLine
    return (read value :: Int)
    
startGame :: IO ()
startGame = do
  stdGen <- newStdGen
  p1 <- player_name 1
  p2 <- player_name 2
  v  <- threshold_value
  let winner = game (map diceRoll (randoms stdGen)) v p1 p2
  putStrLn $ name winner ++ " выиграл счет: " ++ show (score winner)
